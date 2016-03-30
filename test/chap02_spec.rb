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
  it 'オブジェクト指向プログラミングのテスト' do
    class User
      @@count = 0
      def initialize(name)
        @name = name
        @@count = @@count + 1
      end
      attr_accessor :name
      def self.count
        @@count
      end
    end
    expect(User.count).to eq 0
  end
end
