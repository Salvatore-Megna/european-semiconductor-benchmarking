"""Translate titles and legends in recovered historical charts, without redrawing data."""
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "report/figures"
DEST = ROOT / "report/en/figures"
DEST.mkdir(parents=True, exist_ok=True)
FONT = "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
BOLD = "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf"


def label(image, rect, text, size, bold=False, center=False):
    d = ImageDraw.Draw(image)
    d.rectangle(rect, fill="white")
    font = ImageFont.truetype(BOLD if bold else FONT, size)
    tw = d.textbbox((0, 0), text, font=font)[2]
    x = rect[0] + (rect[2] - rect[0] - tw) // 2 if center else rect[0] + 3
    y = rect[1] + max(0, (rect[3] - rect[1] - size) // 2) - 3
    d.text((x, y), text, fill=(0, 0, 0), font=font)


def paired_chart(file, heading, left, right):
    original = Image.open(SRC / file).convert("RGB")
    assert original.size == (2048, 1228)
    image = original.copy()
    label(image, (55, 5, 1800, 68), heading, 41, bold=True)
    label(image, (55, 68, 1840, 107), "The black dashed line shows the annual sample median", 28)
    label(image, (290, 116, 790, 168), left, 28, bold=True, center=True)
    label(image, (1440, 116, 1975, 168), right, 28, bold=True, center=True)
    label(image, (622, 1100, 774, 1188), "Company", 27)
    # Non toccare nessun pixel degli assi e delle serie storiche dei due pannelli.
    assert original.crop((35, 174, 2000, 1060)).tobytes() == image.crop((35, 174, 2000, 1060)).tobytes()
    dest = DEST / file.replace(".png", "_en.png")
    image.save(dest, optimize=True)
    print(dest, "plot pixels preserved")


def pca_chart():
    original = Image.open(SRC / "04_pca_mappa_competitiva.png").convert("RGB")
    assert original.size == (2048, 1479)
    image = original.copy()
    label(image, (105, 6, 1980, 82), "Competitive positioning in the PCA plane", 41, bold=True)
    label(image, (105, 82, 1815, 135), "Company-year observations, 2021–2025", 32)
    label(image, (800, 1215, 1450, 1283), "PC1 (46.2% of variance)", 30, center=True)
    label(image, (595, 1360, 790, 1440), "Company", 29)
    ImageDraw.Draw(image).rectangle((0, 375, 95, 1045), fill="white")
    font = ImageFont.truetype(FONT, 30)
    text = "PC2 (26.5% of variance)"
    box = font.getbbox(text)
    layer = Image.new("RGBA", (box[2] - box[0] + 10, 50), "white")
    ImageDraw.Draw(layer).text((5, 0), text, font=font, fill="black")
    image.paste(layer.rotate(90, expand=True), (13, 410))
    label(image, (1228, 641, 1300, 710), "R&D", 29, bold=True)
    label(image, (935, 386, 1038, 450), "ΔREV", 28, bold=True)
    # Conserva l'intera geometria delle osservazioni e delle frecce PCA.
    for rect in ((105, 140, 918, 1125), (1310, 140, 2025, 1125)):
        assert original.crop(rect).tobytes() == image.crop(rect).tobytes()
    dest = DEST / "04_pca_mappa_competitiva_en.png"
    image.save(dest, optimize=True)
    print(dest, "PCA points and trajectories preserved")


if __name__ == "__main__":
    paired_chart("01_crescita_innovazione.png", "Revenue and R&D intensity over time",
                 "Indexed revenue (2020 = 100)", "R&D intensity (%)")
    paired_chart("02_redditivita_efficienza.png", "Profitability and asset utilization over time",
                 "ROS", "Asset turnover")
    paired_chart("03_struttura_finanziaria.png", "Financial structure over time",
                 "Equity autonomy", "Current ratio")
    pca_chart()
