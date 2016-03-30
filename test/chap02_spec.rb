# -*- coding: utf-8 -*-

describe '四則演算' do 
  it '1 + 2 = 3' do
    expect(1 + 2).to eq 3
  end
end

describe '第2章' do
  it '関数のテスト' do
    adder = lambda do |n|
      lambda do |m|
        n + m
      end
    end
    expect(
           adder[1][2]
           ).to eq 3
  end
end
