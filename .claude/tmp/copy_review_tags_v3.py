#!/usr/bin/env python3
"""
JSファイルからTSファイルへReVIEWタグをコピーするスクリプト (v3)
ペアベースのアプローチ
"""
import re
import os
from pathlib import Path
from typing import List, Tuple, Optional, Dict
from difflib import SequenceMatcher

def normalize_code(line: str) -> str:
    """コード行を正規化"""
    s = line.strip()
    # var/const/letの削除
    s = re.sub(r'\b(var|const|let)\s+', '', s)
    # 型注釈の削除（関数引数など）
    s = re.sub(r':\s*[A-Za-z_][\w<>\[\]\(\)\s,|&\.\?]*(?=[,\)\{=]|$)', '', s)
    # (next) コールバックの削除
    s = re.sub(r'\(next\)', '()', s)
    # to.eql -> toEqual
    s = re.sub(r'\.to\.eql\(', '.toEqual(', s)
    # スペースの正規化
    s = re.sub(r'\s+', ' ', s)
    return s.strip()

def extract_tag_pairs(js_lines: List[str]) -> List[dict]:
    """JSファイルからタグペアを抽出"""
    tag_pattern = re.compile(r'/\*\s*#@range_(begin|end)\(([^)]+)\)\s*\*/')

    # タグ情報を収集
    tag_info = {}
    for i, line in enumerate(js_lines):
        match = tag_pattern.search(line)
        if match:
            tag_type = match.group(1)  # begin or end
            tag_name = match.group(2)
            tag_text = match.group(0)

            if tag_name not in tag_info:
                tag_info[tag_name] = {}

            tag_info[tag_name][tag_type] = {
                'line_num': i,
                'tag_text': tag_text,
                'indent': len(line) - len(line.lstrip())
            }

    # ペアを作成
    pairs = []
    for name, info in tag_info.items():
        if 'begin' in info and 'end' in info:
            begin_line = info['begin']['line_num']
            end_line = info['end']['line_num']

            # 内部のコード（タグ行を除く）
            inner_lines = []
            for i in range(begin_line + 1, end_line):
                inner_lines.append(js_lines[i])

            # 最初と最後の非空白行
            first_code_line = None
            last_code_line = None

            for line in inner_lines:
                if line.strip() and not tag_pattern.search(line):
                    first_code_line = line.strip()
                    break

            for line in reversed(inner_lines):
                if line.strip() and not tag_pattern.search(line):
                    last_code_line = line.strip()
                    break

            pairs.append({
                'name': name,
                'begin': info['begin'],
                'end': info['end'],
                'inner_lines': inner_lines,
                'first_code_line': first_code_line,
                'last_code_line': last_code_line,
                'line_count': end_line - begin_line - 1
            })
        elif 'begin' in info:
            # end のみ不足
            print(f"  警告: {name} のendタグが見つかりません")
        elif 'end' in info:
            # begin のみ不足
            print(f"  警告: {name} のbeginタグが見つかりません")

    return pairs

def find_code_block_in_ts(ts_lines: List[str], pair: dict, used_ranges: List[Tuple[int, int]]) -> Optional[Tuple[int, int]]:
    """TSファイル内でコードブロックを見つける"""
    if not pair['first_code_line'] or not pair['last_code_line']:
        return None

    first_normalized = normalize_code(pair['first_code_line'])
    last_normalized = normalize_code(pair['last_code_line'])

    candidates = []

    for i, ts_line in enumerate(ts_lines):
        ts_normalized = normalize_code(ts_line)

        # 最初の行をマッチング
        if SequenceMatcher(None, ts_normalized, first_normalized).ratio() > 0.8:
            # 最後の行を探す
            expected_end = i + pair['line_count']
            search_range = range(max(i + 1, expected_end - 10), min(len(ts_lines), expected_end + 10))

            for j in search_range:
                end_normalized = normalize_code(ts_lines[j])
                if SequenceMatcher(None, end_normalized, last_normalized).ratio() > 0.8:
                    # このレンジが使用済みでないか確認
                    is_overlapping = any(
                        not (j < ur[0] or i > ur[1])
                        for ur in used_ranges
                    )
                    if not is_overlapping:
                        candidates.append((i, j))

    # 最も長さが近いものを選択
    if candidates:
        target_length = pair['line_count']
        best = min(candidates, key=lambda x: abs((x[1] - x[0]) - target_length))
        return best

    return None

def already_has_tag(ts_lines: List[str], tag_name: str) -> bool:
    """TSファイルにすでにタグがあるか"""
    pattern = f'#@range_(begin|end)\\({re.escape(tag_name)}\\)'
    for line in ts_lines:
        if re.search(pattern, line):
            return True
    return False

def insert_tag_pairs(ts_lines: List[str], pairs: List[dict]) -> Tuple[List[str], int, List[dict]]:
    """タグペアを挿入"""
    result = ts_lines.copy()
    inserted = 0
    failed = []
    used_ranges = []

    # ペアを位置でソート
    pairs_with_pos = []
    for pair in pairs:
        if already_has_tag(result, pair['name']):
            continue

        pos = find_code_block_in_ts(result, pair, used_ranges)
        if pos:
            pairs_with_pos.append((pos, pair))
            used_ranges.append(pos)
        else:
            failed.append(pair)

    # 逆順でソートして挿入
    pairs_with_pos.sort(key=lambda x: x[0][0], reverse=True)

    for (start, end), pair in pairs_with_pos:
        # end タグを挿入（先に）
        end_indent = ' ' * pair['end']['indent']
        result.insert(end + 1, f"{end_indent}{pair['end']['tag_text']}\n")

        # begin タグを挿入
        begin_indent = ' ' * pair['begin']['indent']
        result.insert(start, f"{begin_indent}{pair['begin']['tag_text']}\n")

        inserted += 2

    return result, inserted, failed

def process_file_pair(js_path: str, ts_path: str, verbose: bool = False) -> dict:
    """ファイルペアを処理"""
    print(f"処理中: {os.path.basename(js_path)} -> {os.path.basename(ts_path)}")

    with open(js_path, 'r', encoding='utf-8') as f:
        js_lines = f.readlines()

    with open(ts_path, 'r', encoding='utf-8') as f:
        ts_lines = f.readlines()

    # ペアを抽出
    pairs = extract_tag_pairs(js_lines)
    print(f"  {len(pairs)}個のタグペアを抽出")

    if not pairs:
        return {'extracted': 0, 'inserted': 0, 'failed': 0}

    # タグを挿入
    new_lines, inserted, failed = insert_tag_pairs(ts_lines, pairs)

    # 書き込み
    with open(ts_path, 'w', encoding='utf-8') as f:
        f.writelines(new_lines)

    print(f"  {inserted}個のタグを挿入、{len(failed)}ペアが失敗")

    if verbose and failed:
        for pair in failed[:5]:
            print(f"    失敗: {pair['name']}")
            print(f"      最初の行: {pair['first_code_line'][:60] if pair['first_code_line'] else 'None'}...")
            print(f"      最後の行: {pair['last_code_line'][:60] if pair['last_code_line'] else 'None'}...")

    return {'extracted': len(pairs) * 2, 'inserted': inserted, 'failed': len(failed)}

def main():
    test_dir = Path('/home/emile/projects/functionaljs/test')
    js_files = sorted(test_dir.glob('*.spec.js'))

    total = {'extracted': 0, 'inserted': 0, 'failed': 0}

    for js_file in js_files:
        ts_file = js_file.with_suffix('.ts')
        if ts_file.exists():
            result = process_file_pair(str(js_file), str(ts_file), verbose=True)
            total['extracted'] += result['extracted']
            total['inserted'] += result['inserted']
            total['failed'] += result['failed']
        else:
            print(f"スキップ: {js_file.name} (TSファイルなし)")

    print(f"\n完了!")
    print(f"  抽出: {total['extracted']}")
    print(f"  挿入: {total['inserted']}")
    print(f"  失敗: {total['failed']}")

if __name__ == '__main__':
    main()
