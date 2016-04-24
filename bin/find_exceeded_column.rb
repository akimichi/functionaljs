#! /usr/bin/env ruby
# -*- coding: utf-8 -*-

Dir::glob("*.re").each {|re|
  # ここにマッチしたファイルに対して行う処理を記述する
  # この例ではファイル名とファイルのサイズを標準出力へ出力している
  # puts "#{re}: #{File::stat(re).size} bytes"
  lines = 0
  # File.foreach(re) do |line|
  #   # puts "#{line}"
  #   if(line.length > 80)
  #     puts "#{re}:#{}" 
  # end  
  open(re) {|file|
    while line = file.gets
      lines += 1
      if(line.length > 80)
        puts "#{re}:#{lines}: #{line}" 
      end
    end
  }
}
