#!/usr/bin/env python3
"""
JSファイルからTSファイルへReVIEWタグをコピーするスクリプト (v2)
アンカーベースのアプローチ
"""
import re
import os
from pathlib import Path
from typing import List, Tuple, Optional, Dict
from difflib import SequenceMatcher

def extract_identifier(line: str) -> str:
    """行から識別子（関数名、変数名など）を抽出"""
    # コメント行
    if line.strip().startswith('//') or line.strip().startswith('/*'):
        return line.strip()

    # describe/it
    match = re.search(r"(describe|it)\s*\(\s*['\"]([^'\"]+)['\"]", line)
    if match:
        return f"{match.group(1)}('{match.group(2)}')"

    # 変数/関数定義
    match = re.search(r'(?:var|const|let)\s+(\w+)\s*=', line)
    if match:
        return f"def:{match.group(1)}"

    # 関数呼び出し
    match = re.search(r'(\w+)\s*\(', line)
    if match:
        return f"call:{match.group(1)}"

    # その他
    return line.strip()[:50]

def normalize_code(line: str) -> str:
    """コード行を正規化"""
    # var/const/letの削除
    s = re.sub(r'\b(var|const|let)\s+', '', line)
    # 型注釈の削除
    s = re.sub(r':\s*[A-Za-z_][\w<>\[\]\(\)\s,|&\.\?]*(?=[,\)\{=]|$)', '', s)
    # スペースの正規化
    s = re.sub(r'\s+', ' ', s.strip())
    return s

def similarity(s1: str, s2: str) -> float:
    """2つの文字列の類似度"""
    n1 = normalize_code(s1)
    n2 = normalize_code(s2)
    if n1 == n2:
        return 1.0
    return SequenceMatcher(None, n1, n2).ratio()

def extract_tags_with_anchors(js_lines: List[str]) -> List[dict]:
    """JSファイルからタグとアンカー情報を抽出"""
    tags = []
    tag_pattern = re.compile(r'/\*\s*(#@range_(begin|end)\([^)]+\))\s*\*/')

    for i, line in enumerate(js_lines):
        match = tag_pattern.search(line)
        if match:
            tag_text = match.group(0)
            tag_name = match.group(1)
            is_begin = 'range_begin' in tag_name

            # アンカー行を探す
            anchor_line = None
            anchor_offset = 0

            if is_begin:
                # 後ろのアンカーを探す
                for j in range(i + 1, min(len(js_lines), i + 10)):
                    candidate = js_lines[j].strip()
                    if candidate and not tag_pattern.search(candidate):
                        anchor_line = candidate
                        anchor_offset = j - i
                        break
            else:
                # 前のアンカーを探す
                for j in range(i - 1, max(0, i - 10) - 1, -1):
                    candidate = js_lines[j].strip()
                    if candidate and not tag_pattern.search(candidate):
                        anchor_line = candidate
                        anchor_offset = j - i
                        break

            # 周辺コンテキスト（追加のアンカー）
            context = []
            for j in range(max(0, i - 3), min(len(js_lines), i + 4)):
                if j != i and not tag_pattern.search(js_lines[j]):
                    context.append((j - i, js_lines[j].strip()))

            tags.append({
                'line_num': i,
                'tag_text': tag_text,
                'tag_name': tag_name,
                'is_begin': is_begin,
                'anchor_line': anchor_line,
                'anchor_offset': anchor_offset,
                'context': context,
                'indent': len(line) - len(line.lstrip())
            })

    return tags

def find_anchor_in_ts(ts_lines: List[str], tag: dict, used_positions: set) -> Optional[int]:
    """TSファイル内でアンカー位置を見つける"""
    if not tag['anchor_line']:
        return None

    anchor = tag['anchor_line']
    best_match = None
    best_score = 0.0

    for i, ts_line in enumerate(ts_lines):
        # すでに使用された位置に近い場合はスキップ
        if any(abs(i - pos) < 3 for pos in used_positions):
            continue

        sim = similarity(ts_line.strip(), anchor)
        if sim > 0.75 and sim > best_score:
            # コンテキストも確認
            context_score = 0
            context_count = 0
            for offset, ctx_line in tag['context']:
                ctx_i = i + offset
                if 0 <= ctx_i < len(ts_lines):
                    ctx_sim = similarity(ts_lines[ctx_i].strip(), ctx_line)
                    context_score += ctx_sim
                    context_count += 1

            if context_count > 0:
                avg_context_score = context_score / context_count
                combined_score = (sim + avg_context_score) / 2
            else:
                combined_score = sim

            if combined_score > best_score:
                best_score = combined_score
                best_match = i

    if best_match is not None:
        # タグの挿入位置を計算
        if tag['is_begin']:
            return best_match  # アンカー行の前
        else:
            return best_match + 1  # アンカー行の後

    return None

def already_has_tag(ts_lines: List[str], tag_name: str) -> bool:
    """TSファイルにすでに同じタグがあるか確認"""
    for line in ts_lines:
        if tag_name in line:
            return True
    return False

def get_proper_indent(ts_lines: List[str], pos: int) -> str:
    """適切なインデントを取得"""
    if 0 <= pos < len(ts_lines):
        line = ts_lines[pos]
        match = re.match(r'^(\s*)', line)
        if match:
            return match.group(1)
    return ''

def insert_tags(ts_lines: List[str], tags: List[dict], verbose: bool = False) -> Tuple[List[str], int, List[dict]]:
    """TSファイルにタグを挿入"""
    result = ts_lines.copy()
    insertions = []
    used_positions = set()
    failed = []

    # 各タグの挿入位置を決定
    for tag in tags:
        # すでにタグがある場合はスキップ
        if already_has_tag(result, tag['tag_name']):
            if verbose:
                print(f"  スキップ（すでに存在）: {tag['tag_name']}")
            continue

        pos = find_anchor_in_ts(result, tag, used_positions)
        if pos is not None:
            insertions.append((pos, tag))
            used_positions.add(pos)
        else:
            failed.append(tag)
            if verbose:
                print(f"  位置が見つからない: {tag['tag_name']}")
                print(f"    アンカー: {tag['anchor_line'][:60] if tag['anchor_line'] else 'None'}...")

    # 逆順でソートして挿入（行番号のズレを防ぐ）
    insertions.sort(key=lambda x: x[0], reverse=True)

    for pos, tag in insertions:
        indent = get_proper_indent(result, pos)
        tag_line = f"{indent}{tag['tag_text']}\n"
        result.insert(pos, tag_line)

    return result, len(insertions), failed

def process_file_pair(js_path: str, ts_path: str, verbose: bool = False) -> dict:
    """ファイルペアを処理"""
    print(f"処理中: {os.path.basename(js_path)} -> {os.path.basename(ts_path)}")

    with open(js_path, 'r', encoding='utf-8') as f:
        js_lines = f.readlines()

    with open(ts_path, 'r', encoding='utf-8') as f:
        ts_lines = f.readlines()

    # タグを抽出
    tags = extract_tags_with_anchors(js_lines)
    print(f"  JSから{len(tags)}個のタグを抽出")

    if not tags:
        return {'extracted': 0, 'inserted': 0, 'failed': 0}

    # タグを挿入
    new_lines, inserted, failed = insert_tags(ts_lines, tags, verbose)

    # 書き込み
    with open(ts_path, 'w', encoding='utf-8') as f:
        f.writelines(new_lines)

    print(f"  {inserted}個のタグを挿入、{len(failed)}個が失敗")

    return {'extracted': len(tags), 'inserted': inserted, 'failed': len(failed), 'failed_tags': failed}

def main():
    test_dir = Path('/home/emile/projects/functionaljs/test')
    js_files = sorted(test_dir.glob('*.spec.js'))

    total_extracted = 0
    total_inserted = 0
    total_failed = 0

    for js_file in js_files:
        ts_file = js_file.with_suffix('.ts')
        if ts_file.exists():
            result = process_file_pair(str(js_file), str(ts_file), verbose=True)
            total_extracted += result['extracted']
            total_inserted += result['inserted']
            total_failed += result['failed']
        else:
            print(f"スキップ: {js_file.name} (TSファイルなし)")

    print(f"\n完了!")
    print(f"  合計抽出: {total_extracted}")
    print(f"  合計挿入: {total_inserted}")
    print(f"  合計失敗: {total_failed}")

if __name__ == '__main__':
    main()
