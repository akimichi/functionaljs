#!/usr/bin/env python3
"""
JSファイルからTSファイルへReVIEWタグをコピーするスクリプト
"""
import re
import os
import sys
from pathlib import Path
from typing import List, Tuple, Optional
from difflib import SequenceMatcher

def normalize_line(line: str) -> str:
    """行を正規化して比較しやすくする"""
    # コメント内のスペースは保持
    if '#@range_' in line:
        return line

    # 基本的な正規化
    normalized = line.strip()

    # var -> const/let の正規化
    normalized = re.sub(r'\bvar\s+', '', normalized)
    normalized = re.sub(r'\bconst\s+', '', normalized)
    normalized = re.sub(r'\blet\s+', '', normalized)

    # 型注釈の削除
    normalized = re.sub(r':\s*[A-Za-z<>\[\]\(\)\s,|&]+(?=[=,\)\{])', '', normalized)

    # スペースの正規化
    normalized = re.sub(r'\s+', ' ', normalized)

    return normalized

def is_similar(line1: str, line2: str, threshold: float = 0.7) -> bool:
    """2つの行が類似しているか判定"""
    n1 = normalize_line(line1)
    n2 = normalize_line(line2)

    if n1 == n2:
        return True

    # 完全な正規化後の比較
    ratio = SequenceMatcher(None, n1, n2).ratio()
    return ratio >= threshold

def extract_tags_from_js(js_lines: List[str]) -> List[dict]:
    """JSファイルからタグ情報を抽出"""
    tags = []
    tag_pattern = re.compile(r'/\*\s*(#@range_(begin|end)\([^)]+\))\s*\*/')

    for i, line in enumerate(js_lines):
        match = tag_pattern.search(line)
        if match:
            tag_text = match.group(0)
            tag_name = match.group(1)

            # コンテキスト: タグの前後の非空白・非タグ行を取得
            context_before = []
            context_after = []

            # 前のコンテキスト
            for j in range(i - 1, max(0, i - 5) - 1, -1):
                prev_line = js_lines[j].strip()
                if prev_line and not tag_pattern.search(prev_line):
                    context_before.insert(0, prev_line)
                    if len(context_before) >= 2:
                        break

            # 後のコンテキスト
            for j in range(i + 1, min(len(js_lines), i + 5)):
                next_line = js_lines[j].strip()
                if next_line and not tag_pattern.search(next_line):
                    context_after.append(next_line)
                    if len(context_after) >= 2:
                        break

            tags.append({
                'line_num': i,
                'tag_text': tag_text,
                'tag_name': tag_name,
                'context_before': context_before,
                'context_after': context_after,
                'original_line': line
            })

    return tags

def find_insertion_point(ts_lines: List[str], tag: dict) -> Optional[int]:
    """TSファイル内でタグを挿入すべき位置を見つける"""

    # range_beginの場合：context_afterを探す
    # range_endの場合：context_beforeを探す
    is_begin = 'range_begin' in tag['tag_name']

    if is_begin and tag['context_after']:
        # 後のコンテキストを探す
        search_context = tag['context_after']
        for i, ts_line in enumerate(ts_lines):
            # 最初のコンテキスト行と比較
            if is_similar(ts_line.strip(), search_context[0]):
                # 追加のコンテキストも確認
                if len(search_context) > 1 and i + 1 < len(ts_lines):
                    if is_similar(ts_lines[i + 1].strip(), search_context[1]):
                        return i  # この行の前にタグを挿入
                else:
                    return i
    elif not is_begin and tag['context_before']:
        # 前のコンテキストを探す
        search_context = tag['context_before']
        for i, ts_line in enumerate(ts_lines):
            # 最後のコンテキスト行と比較
            if is_similar(ts_line.strip(), search_context[-1]):
                # 追加のコンテキストも確認
                if len(search_context) > 1 and i > 0:
                    if is_similar(ts_lines[i - 1].strip(), search_context[-2]):
                        return i + 1  # この行の後にタグを挿入
                else:
                    return i + 1

    return None

def get_indentation(ts_lines: List[str], insertion_point: int) -> str:
    """挿入位置のインデントを取得"""
    if insertion_point < len(ts_lines):
        line = ts_lines[insertion_point]
        match = re.match(r'^(\s*)', line)
        if match:
            return match.group(1)
    return ''

def insert_tags_to_ts(ts_lines: List[str], tags: List[dict]) -> List[str]:
    """TSファイルにタグを挿入"""
    result = ts_lines.copy()
    inserted_count = 0
    failed_tags = []

    # 挿入位置を計算
    insertions = []
    for tag in tags:
        pos = find_insertion_point(result, tag)
        if pos is not None:
            insertions.append((pos, tag))
        else:
            failed_tags.append(tag)

    # 位置でソート（逆順で挿入するため）
    insertions.sort(key=lambda x: x[0], reverse=True)

    for pos, tag in insertions:
        # すでに同じタグがある場合はスキップ
        tag_name_match = re.search(r'#@range_(begin|end)\(([^)]+)\)', tag['tag_name'])
        if tag_name_match:
            search_str = f'#@range_{tag_name_match.group(1)}({tag_name_match.group(2)})'
            if any(search_str in line for line in result):
                continue

        indent = get_indentation(result, pos)
        tag_line = f"{indent}{tag['tag_text']}\n"
        result.insert(pos, tag_line)
        inserted_count += 1

    if failed_tags:
        print(f"  警告: {len(failed_tags)}個のタグの挿入位置が見つかりませんでした:")
        for tag in failed_tags[:5]:  # 最初の5個だけ表示
            print(f"    - {tag['tag_name']}")
        if len(failed_tags) > 5:
            print(f"    ... and {len(failed_tags) - 5} more")

    return result

def process_file_pair(js_path: str, ts_path: str) -> bool:
    """JSファイルとTSファイルのペアを処理"""
    print(f"処理中: {os.path.basename(js_path)} -> {os.path.basename(ts_path)}")

    with open(js_path, 'r', encoding='utf-8') as f:
        js_lines = f.readlines()

    with open(ts_path, 'r', encoding='utf-8') as f:
        ts_lines = f.readlines()

    # タグを抽出
    tags = extract_tags_from_js(js_lines)
    print(f"  JSファイルから{len(tags)}個のタグを抽出")

    if not tags:
        print("  タグがありません。スキップします。")
        return True

    # タグを挿入
    new_ts_lines = insert_tags_to_ts(ts_lines, tags)

    # 書き込み
    with open(ts_path, 'w', encoding='utf-8') as f:
        f.writelines(new_ts_lines)

    inserted_count = len(new_ts_lines) - len(ts_lines)
    print(f"  {inserted_count}行を挿入しました")

    return True

def main():
    test_dir = Path('/home/emile/projects/functionaljs/test')

    # ペアを見つける
    js_files = list(test_dir.glob('*.spec.js'))

    for js_file in sorted(js_files):
        ts_file = js_file.with_suffix('.ts')
        if ts_file.exists():
            process_file_pair(str(js_file), str(ts_file))
        else:
            print(f"スキップ: {js_file.name} (対応するTSファイルがありません)")

    print("\n完了!")

if __name__ == '__main__':
    main()
