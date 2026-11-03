"""Integrity checks for recovered original datasets, scripts and results."""
from pathlib import Path
import hashlib
import unittest

ROOT = Path(__file__).resolve().parents[1]
CHECKSUMS = {
    "data/raw/benchmark_semiconductor_europe_2020_2025_CORRETTO.xlsx": "ca5bfed2b0d1c8d5d975a6c3c60d25bc165718d49688e802e879121f2f27916c",
    "results/risultati_storici.xlsx": "d9a96a381b342c46ae12a4e32df9fc3cc3878737bc9160d291d8a626f9ad39d3",
    "docs/source_archive/analisi_semiconductor_v7_RDS_2020_CORRETTA.R": "2e5b37c1fba8282e3132941800aa628702c30e3714241367209f231a5fb93679",
    "docs/source_archive/INDICATORI_2_revisionato.tex": "50d8eb64195396eb2107c5f7317c7750c83b4179747ca115d60cbc176e777fa7",
}

class RecoveredSourceTests(unittest.TestCase):
    def test_source_sha256(self):
        for name, expected in CHECKSUMS.items():
            with self.subTest(source=name):
                actual = hashlib.sha256((ROOT / name).read_bytes()).hexdigest()
                self.assertEqual(actual, expected)

if __name__ == "__main__":
    unittest.main()
