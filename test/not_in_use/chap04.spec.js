
  /*
    describe('型決定のタイミング', () => {
    it('NaNはエラーの場所を発見しにくくする', (next) => {
    var one = 1;
    var two = 2;
    var three = "three";
    var four = 4;
    expect(
    one * two * (three + four)
    ).to.eql(
    11   // 1 * 2 + (3 + 4) = 10 を期待する
    );
    next();
    });
    })
  */
    // it('関数の適用', (next) => {
    //   /* #@range_begin(function_application) */
    //   var func = (any) => {
    //     return any;
    //   };
    //   /* #@range_end(function_application) */
    //   expect(
    //     func(1)
    //   ).to.eql(
    //     1
    //   );
    //   next();
    // });



      /* ##@range_begin(embeded_function_invocation) */
      // expect(
      //   addressbook.BMI(addressbook.turing.weight, addressbook.turing.height)
      // ).to.within(21.0,22,0);
      /* ##@range_end(embeded_function_invocation) */


  it('==比較演算子', (next) => {
    /* #@range_begin(equality_operator) */
    expect(
      null == undefined
    ).to.eql(
      true
    );
    expect(
      false == ''
    ).to.eql(
      true
    );
    expect(
      true == '1'
    ).to.eql(
      true
    );
    expect(
      1 == '1'
    ).to.eql(
      true
    );
    /* #@range_end(equality_operator)  */
    next();
  });

    it('for in で反復処理する', (next) => {
      /* ##@range_begin(for_in_object) */
      var obj = {
        one: 1,
        two: 2
      };
      var results = [];
      for (var key in obj) {
        results.push(obj[key])
      }
      expect(
        results
      ).to.eql(
        [1,2]
      );
      /* ##@range_end(for_in_object)     */
      next();
    });
    it('オブジェクトに関数をいれる', (next) => {
      /* ##@range_begin(object_can_embed_function) */
      var addressbook = [
        {
          name: "Alan Turing",
          gender: "male",
          birthDay: "1912/6/23",
          age: (now) => {
            expect(now).to.be.a('object');
            var birthDayObject = new Date(this.birthDay);
            return (now.getFullYear() - birthDayObject.getFullYear());
          }
        },
        {
          name: "Jane Austen",
          gender: "female",
          birthDay: "1775/12/16"
        },
        {
          name: "Fyodor Dostoyevsky",
          gender: "male",
          birthDay: "1821/11/11"
        },
        {
          name: "Ada Lovelace",
          gender: "female",
          birthDay: "1815/12/10"
        }
      ];
      expect(
        addressbook[0].age(new Date())
      ).to.be(104);
      /* ##@range_end(object_can_embed_function) */
      next();
    });
    it('抽象データ型としてのスタック', (next) => {
      var push = (n, stack) => {
        return [n].concat(stack);
      };
      var top = (stack) => {
        return stack[0];
      };
      var pop = (stack) => {
        return stack.slice(1,stack.length);
      };
      var empty = [];
      /* #@range_begin(stack_as_abstract_type) */
      expect(
        top(pop(pop(push(3,push(2,push(1,empty))))))
      ).to.eql(
        1
      );
      /* #@range_end(stack_as_abstract_type) */
      next();
    });


    it('スコープの入れ子', (next) => {
      /* #@range_begin(nested_scope) */
      var outerScope = 1;
      var createScope = (_) =>  {
        var innerScope = 2;
        return innerScope + outerScope; // 内側のスコープから外側のスコープにある outerScope変数にアクセスしている
      };
      /* #@range_end(nested_scope) */
      expect(
        outerScope
      ).to.eql(
        1
      );
      expect(
        createScope()
      ).to.eql(
        3
      );
      next();
    });
    it('ホイスティング', (next) => {
      /* #@range_begin(hoisting) */
      var scope = "global";
      var f = (_) =>  {
        expect(
          scope
        ).to.be(
          "global"
        );
      };
      /* #@range_end(hoisting) */
      next();
    });

    it('クロージャーによるスコープの入れ子', (next) => {
      /* #@range_begin(nested_scope_in_closure) */
      var outerFunction = (outerArgument) => {
        var innerFunction = (innerArgument) => {
          return outerArgument + innerArgument;
        };
        return innerFunction;
      };
      /* #@range_end(nested_scope_in_closure) */
      next();
    });
    it('内側のスコープから外側のスコープの参照', (next) => {
      /* #@range_begin(function_scope_nesting) */
      var scope = "outer";
      var func = (_) =>  {
        return scope;
      };
      expect(
        func()
      ).to.be(
        "outer"
      );
      /* #@range_end(function_scope_nesting) */
      next();
    });
  describe('代入の仕組み', () => {
    it('基本型への代入', (next) => {
      /* #@range_begin(assign_to_basic_value) */
      var basic_value = 1;
      expect(
        basic_value
      ).to.eql(
        1
      );
      basic_value = 3;
      expect(
        basic_value
      ).to.eql(
        3
      );
      /* #@range_end(assign_to_basic_value) */
      next();
    });
  });

    it('合成型への代入', (next) => {
      /* #@range_begin(assign_to_complex_value) */
      var object = {one: 1, two: 2};
      expect(
        object.one
      ).to.eql(
        1
      );
      object.one = 3;
      expect(
        object.one
      ).to.eql(
        3
      );
      /* #@range_end(assign_to_complex_value) */
      next();
    });

    // it('合成型は参照である', (next) => {
    //   /* #@range_begin(complex_type_is_reference) */
    //   var array = [0,1,2,3];
    //   expect(
    //     array
    //   ).not.to.eql(
    //     [0,1,2,3]
    //   );
    //   var object = { key: 1 };
    //   expect(
    //     object
    //   ).not.to.eql(
    //     {
    //       key: 1
    //     }
    //   );
    //   var func = (_) => { return 1; };
    //   expect(
    //     func
    //   ).not.to.eql(
    //     ((_) => { return 1; })
    //   );
    //   /* #@range_end(complex_type_is_reference) */
    //   next();
    // });
  // ### 参照透過性を成立させる条件
  describe('参照透過性を成立させる条件', () => {
    it('参照透過性がある場合', (next) => {
      /* #@range_begin(referential_transparency_example) */
      var x = 1;
      var y = 2;
      var z = x + y;
      expect(
        (x + y) * (x + y)
      ).to.be(
        z * z // (x + y) を z に置きかえた
      );
      /* #@range_end(referential_transparency_example) */
      next();
    });
    it('可変なデータによって参照透過性を失う場合', (next) => {
      /* #@range_begin(referential_transparency_counterexample_mutable) */
      var x = 1;
      var y = 2;
      var z = [x,y];
      expect(
        [x,y]
      ).not.to.be( // notを使っている
        z // [x,y] を z に置きかえた
      );
      /* #@range_end(referential_transparency_counterexample_mutable) */
      next();
    });
  });
