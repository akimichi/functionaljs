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
    user_a = User.new("a")
    expect(User.count).to eq 1
    user_b = User.new("b")
    expect(User.count).to eq 2
    
    class Player < User
      @score = 0
      def win
        @score = @score + 1
      end
      attr_accessor :score
    end
  end
  describe '銀行口座の例' do
    #@range_begin(account_with_implicit_state)
    class Account             # 銀行口座のクラス
      def initialize(balance) # 銀行口座のインスタンスを作る
        @balance = balance
      end
      attr_accessor :balance
      def deposit(amount)     # 口座にお金を預ける
        @balance = @balance + amount
      end
      def withdraw(amount)    # 口座からお金を引き出す
        @balance = @balance - amount
      end
    end
    #@range_end(account_with_implicit_state)
    it '浪費家の例' do
      #@range_begin(account_waster_test)
      account = Account.new(100)
      expect(account.balance).to eq 100
      account.withdraw(60)
      expect(account.balance).to eq 40
      account.withdraw(60)
      expect(account.balance).to eq -20
      #@range_end(account_waster_test)
    end
    it '吝嗇家の例' do
      account = Account.new(100)
      expect(account.balance).to eq 100
      account.deposit(20)
      expect(account.balance).to eq 120
      account.withdraw(40)
      expect(account.balance).to eq 80
    end
  end
end
