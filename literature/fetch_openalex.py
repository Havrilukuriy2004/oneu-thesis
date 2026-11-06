"""
fetch_openalex.py — пошук літератури для розділу 2 (OpenAlex), запускати на своєму комп'ютері.
Не потребує встановлення пакетів (лише стандартна бібліотека Python 3).

    cd ~/Documents/master_thesis_section2/literature
    python3 fetch_openalex.py

Результат: openalex_results.json у цій самій папці (Claude забере його сам).
Необов'язково: ключ OpenAlex (https://openalex.org/settings/api) у змінній середовища OPENALEX_API_KEY.
"""
import json, os, time, urllib.parse, urllib.request

API_KEY = os.environ.get("OPENALEX_API_KEY", "")
BASE = "https://api.openalex.org"
SELECT = "id,doi,title,publication_year,cited_by_count,authorships,primary_location,abstract_inverted_index,open_access,type,referenced_works_count"
UA = {"User-Agent": "master-thesis-literature-review (python urllib)"}


def get(path, params):
    params = dict(params)
    if API_KEY:
        params["api_key"] = API_KEY
    url = f"{BASE}{path}?{urllib.parse.urlencode(params, safe=':,|>')}"
    for attempt in range(4):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=40) as r:
                time.sleep(0.4)
                return json.loads(r.read().decode("utf-8"))
        except Exception as e:  # rate limit / transient
            print(f"   retry {attempt + 1}: {e}")
            time.sleep(3 * (attempt + 1))
    return {"results": []}


def works(search, extra_filter="", sort="cited_by_count:desc", n=25):
    f = "type:article|preprint|report" if not extra_filter else f"type:article|preprint|report,{extra_filter}"
    return get("/works", {"search": search, "filter": f, "sort": sort, "per_page": n, "select": SELECT}).get("results", [])


# keyword matrix: section 2.2 (pass-through), 2.3 (repricing gap / NII, IRRBB), 2.4 (shadow funding cost:
# reserve requirements, deposit insurance, dollarisation), allocation (CDs / sovereign bonds vs loans), Ukraine
QUERIES = {
    # 2.2 transmission
    "q01_passthrough": ("interest rate pass-through bank lending deposit rates", ""),
    "q02_passthrough_recent": ("interest rate pass-through bank rates monetary policy", "publication_year:>2018"),
    "q03_deposits_channel": ("deposits channel monetary policy deposit spreads market power", ""),
    "q04_asymmetry": ("asymmetric adjustment deposit rates rigidity banks", ""),
    "q05_regime_switching": ("Markov switching interest rate pass-through", ""),
    "q06_bank_lending_channel": ("bank lending channel monetary policy bank balance sheet heterogeneity", ""),
    "q07_expectations_surprises": ("monetary policy surprises bank interest rates expectations", ""),
    # 2.3 repricing gap / NII / IRRBB
    "q11_income_gap": ("income gap repricing gap bank net interest income monetary policy", ""),
    "q12_irrbb": ("interest rate risk banking book economic value of equity net interest income sensitivity", ""),
    "q13_nim_rates": ("bank profitability net interest margin interest rate level", ""),
    "q14_maturity_transformation": ("maturity transformation interest rate risk deposit franchise hedging", ""),
    "q15_nonmaturity_deposits": ("non-maturity deposits duration behavioral model valuation", ""),
    "q16_rate_exposure_stock": ("bank stock returns interest rate risk exposure", ""),
    # 2.4 shadow funding cost
    "q21_reserve_requirements": ("reserve requirements monetary policy instrument emerging markets", ""),
    "q22_rr_tax": ("reserve requirements implicit tax bank funding cost deposit rates", ""),
    "q23_dollarization_rr": ("financial dollarization deposits reserve requirements currency", ""),
    "q24_deposit_insurance_cost": ("deposit insurance premium bank funding cost pricing", ""),
    "q25_funding_cost_wholesale": ("bank funding costs composition deposits wholesale monetary policy", ""),
    # allocation: CB bills / sovereign bonds vs loans
    "q31_cb_bills": ("central bank bills sterilization bank lending crowding out", ""),
    "q32_sovereign_crowding": ("bank holdings of government bonds crowding out lending sovereign", ""),
    "q33_liquidity_regulation": ("liquidity coverage ratio bank asset allocation lending", ""),
    "q34_excess_reserves_remuneration": ("interest on reserves remuneration bank lending deposit rates", ""),
    # Ukraine / war
    "q41_ukraine_transmission": ("Ukraine monetary transmission interest rates banks", ""),
    "q42_ukraine_banks": ("Ukrainian banks deposits lending National Bank of Ukraine", ""),
    "q43_war_banking": ("war banking system deposits lending conflict", "publication_year:>2014"),
}

AUTHORS = ["Itamar Drechsler", "Leonardo Gambacorta", "Juliane Begenau", "Anil Kashyap", "Skander Van den Heuvel",
           "Matthias Hoffmann", "Ugo Albertazzi", "Carlo Altavilla"]

out = {"queries": {}, "authors": {}}
for k, (q, flt) in QUERIES.items():
    print(f"[{k}] {q}")
    res = works(q, flt)
    out["queries"][k] = {"query": q, "filter": flt, "results": res}
    print(f"   -> {len(res)}")

for name in AUTHORS:
    a = get("/authors", {"search": name, "per_page": 3}).get("results", [])
    if not a:
        continue
    aid = a[0]["id"].split("/")[-1]
    res = get("/works", {"filter": f"authorships.author.id:{aid}", "sort": "cited_by_count:desc", "per_page": 15, "select": SELECT}).get("results", [])
    out["authors"][name] = {"author_id": aid, "display_name": a[0].get("display_name"), "results": res}
    print(f"[author] {name} ({aid}) -> {len(res)}")

with open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "openalex_results.json"), "w", encoding="utf-8") as f:
    json.dump(out, f, ensure_ascii=False)
n = sum(len(v["results"]) for v in out["queries"].values()) + sum(len(v["results"]) for v in out["authors"].values())
print(f"\nГотово: openalex_results.json ({n} записів). Напишіть Claude «літературу завантажив».")
