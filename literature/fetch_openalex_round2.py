"""
fetch_openalex_round2.py — другий, точніший раунд пошуку (точні фрази в назві/анотації + мережа цитувань
ключових праць). Запуск:  cd ~/Documents/master_thesis_section2/literature && python3 fetch_openalex_round2.py
Результат: openalex_round2.json
"""
import json, os, time, urllib.parse, urllib.request

API_KEY = os.environ.get("OPENALEX_API_KEY", "")
BASE = "https://api.openalex.org"
SELECT = "id,doi,title,publication_year,cited_by_count,authorships,primary_location,abstract_inverted_index,open_access,type"
UA = {"User-Agent": "master-thesis-literature-review (python urllib)"}


def get(path, params):
    params = dict(params)
    if API_KEY:
        params["api_key"] = API_KEY
    url = f"{BASE}{path}?{urllib.parse.urlencode(params, safe=':,|>\"')}"
    for attempt in range(4):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=40) as r:
                time.sleep(0.35)
                return json.loads(r.read().decode("utf-8"))
        except Exception as e:
            print(f"   retry {attempt + 1}: {e}")
            time.sleep(3 * (attempt + 1))
    return {"results": []}


def phrase(q, n=25, sort="cited_by_count:desc", extra=""):
    f = f'title_and_abstract.search:{q}'
    if extra:
        f += "," + extra
    return get("/works", {"filter": f, "sort": sort, "per_page": n, "select": SELECT}).get("results", [])


# exact phrases (OpenAlex: quotes = phrase); several per topic
PHRASES = {
    # 2.2
    "p_passthrough_retail": '"interest rate pass-through" bank',
    "p_passthrough_deposit": '"pass-through" "deposit rates"',
    "p_passthrough_emerging": '"interest rate pass-through" emerging',
    "p_deposit_beta": '"deposit beta"',
    "p_deposit_spreads": '"deposit spreads" monetary policy',
    "p_deposit_rate_rigidity": '"deposit rate" rigidity',
    "p_markov_passthrough": '"pass-through" "Markov switching"',
    "p_threshold_passthrough": '"pass-through" asymmetric threshold bank rates',
    "p_policy_surprise_bank_rates": '"monetary policy surprises" bank lending rates',
    # 2.3
    "p_income_gap": '"income gap" banks',
    "p_repricing_gap": '"repricing gap"',
    "p_irrbb": '"interest rate risk in the banking book"',
    "p_nii_sensitivity": '"net interest income" sensitivity interest rates banks',
    "p_eve": '"economic value of equity"',
    "p_deposit_franchise": '"deposit franchise"',
    "p_nmd": '"non-maturity deposits"',
    "p_maturity_mismatch": '"maturity mismatch" banks interest rate risk',
    # 2.4
    "p_reserve_req_tool": '"reserve requirements" monetary policy',
    "p_reserve_req_lending": '"reserve requirements" bank lending',
    "p_reserve_req_tax": '"reserve requirements" tax',
    "p_reserve_req_dollar": '"reserve requirements" dollarization',
    "p_dollarization_deposits": '"deposit dollarization"',
    "p_dollarization_financial": '"financial dollarization" banks',
    "p_dgf_premium": '"deposit insurance" premiums banks cost',
    "p_cb_bills": '"central bank bills"',
    "p_sterilization_banks": 'sterilization bank lending emerging',
    "p_sovereign_crowding": '"crowding out" bank lending government bonds',
    "p_sovereign_holdings": '"sovereign bonds" banks holdings lending',
    "p_remuneration_reserves": '"interest on reserves" deposit rates',
    "p_liquidity_surplus": '"structural liquidity surplus"',
    "p_corridor_floor": '"floor system" monetary policy banks',
    # Ukraine / war
    "p_ukraine_monetary": 'Ukraine "monetary policy" transmission',
    "p_ukraine_banking": 'Ukraine banking sector',
    "p_ukraine_inflation_targeting": 'Ukraine "inflation targeting"',
    "p_ukraine_deposits": 'Ukraine deposits banks',
    "p_ukraine_dollarization": 'Ukraine dollarization',
    "p_war_banks": 'war banks deposits',
}

# seeds for citation network (forward citations, most cited + most recent)
SEEDS = {
    "DSS2017_deposits_channel": "W3121380054",
    "DSS2021_banking_on_deposits": "W3125262113",
    "EVZ2018_irr_equity": "W1515856918",
    "BGH2017_bank_profitability": "W2192993715",
    "KS2000_million_obs": "W2134443358",
    "Gambacorta2008_how_banks_set_rates": "W2076875561",
    "Claessens2017_low_for_long": "W2590767792",
    "Altavilla2018_profitability": "W2766032105",
    "Kuttner2001_surprises": "W3124933125",
    "DSS2023_deposit_franchise_runs": "W4366137419",
}

out = {"phrases": {}, "forward": {}, "forward_recent": {}}
for k, q in PHRASES.items():
    res = phrase(q)
    out["phrases"][k] = {"q": q, "results": res}
    print(f"[{k}] {len(res)}")
    res2 = phrase(q, n=15, sort="publication_year:desc", extra="publication_year:>2019")
    out["phrases"][k + "__recent"] = {"q": q, "results": res2}

for name, wid in SEEDS.items():
    res = get("/works", {"filter": f"cites:{wid}", "sort": "cited_by_count:desc", "per_page": 25, "select": SELECT}).get("results", [])
    out["forward"][name] = {"id": wid, "results": res}
    res2 = get("/works", {"filter": f"cites:{wid},publication_year:>2021", "sort": "cited_by_count:desc", "per_page": 15, "select": SELECT}).get("results", [])
    out["forward_recent"][name] = {"id": wid, "results": res2}
    print(f"[cites {name}] {len(res)} + recent {len(res2)}")

with open(os.path.join(os.path.dirname(os.path.abspath(__file__)), "openalex_round2.json"), "w", encoding="utf-8") as f:
    json.dump(out, f, ensure_ascii=False)
print("\nГотово: openalex_round2.json. Напишіть Claude «раунд 2 готовий».")
