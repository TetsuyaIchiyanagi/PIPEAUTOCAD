"""XXPLODEの構文・ロード/メニュー接続を検査する。AutoCAD実機試験とは別。"""
from pathlib import Path

ROOT = Path(__file__).resolve().parent


def forms(path):
    text = path.read_text(encoding="utf-8-sig")
    depth = 0
    quoted = escaped = comment = False
    for line, row in enumerate(text.splitlines(), 1):
        comment = False
        for ch in row:
            if comment:
                continue
            if quoted:
                if escaped:
                    escaped = False
                elif ch == "\\":
                    escaped = True
                elif ch == '"':
                    quoted = False
            elif ch == ";":
                comment = True
            elif ch == '"':
                quoted = True
            elif ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                assert depth >= 0, f"{path.name}:{line}: extra closing parenthesis"
    assert depth == 0 and not quoted, f"{path.name}: unclosed form/string ({depth})"
    print(f"PASS: {path.name} balanced forms and strings")
    return text


if __name__ == "__main__":
    source = forms(ROOT / "xxplode.lsp")
    main = forms(ROOT / "main.LSP")
    forms(ROOT / "xxplode_regression.lsp")
    assert '(load (strcat pa-path "xxplode.lsp"))' in main
    assert "(defun c:MAIN " in main and "c:XXPLODE" in main
    assert "(defun c:XXPLODE () (XXP-run))" in source
    print("PASS: loader, MAIN menu and standalone entry point connected")
