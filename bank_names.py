"""Presentation labels only. NBU bank identifiers remain unchanged."""
import re

def clean_bank_name(value):
    return re.sub(r'\s+', ' ', re.sub(r'["«»„“”]', '', str(value or ''))).strip()
