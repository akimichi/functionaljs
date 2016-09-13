
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

      expect(
        str.toUpperCase()
      ).to.eql(
        "TO BE, OR NOT TO BE"
      );
      // it('オブジェクト型によるアドレス帳の表現', (next) => {
      //   /* #@range_begin(addressbook_example) */
      //   var addressbook = {
      //     BMI: (weight, height) => {
      //       expect(weight).to.be.a('number');
      //       expect(height).to.be.a('number');
      //       return weight / ((height / 100.0) * (height / 100.0));
      //     },
      //     No1: {
      //       name: "Alan Turing",
      //       birthDay: "1912/6/23",
      //       weight: 62,
      //       height: 175
      //     },
      //     No2: {
      //       name: "Haskell Curry",
      //       birthDay: "1900/9/12",
      //       weight: 62,
      //       height: 180
      //     },
      //     No3: {
      //       name: "Alonzo Church",
      //       birthDay: "1903/6/14",
      //       weight: 75,
      //       height: 168
      //     }
      //   };
      //   expect(
      //     addressbook.No1.name
      //   ).to.eql(
      //     "Alan Turing"
      //   );
      //   expect(
      //     addressbook.BMI(
      //       addressbook.No1.weight,
      //       addressbook.No1.height)
      //   ).to.be.within(20.0,21.0); // within(A,B) は値が AとBのあいだにはいっていることをチェックする
      //   /* #@range_end(addressbook_example) */
      //   next();
      // });

        // /* hasOwnPropertyでプロパティの有無を調べる */
        // expect(
        //   addressbook.hasOwnProperty("No1")
        // ).to.eql(
        //   true
        // );
        // expect(
        //   addressbook.hasOwnProperty("No9")
        // ).to.eql(
        //   false
        // );

    // it("Array.forEach文", (next) => {
    //   /* #@range_begin(forEach_in_array) */
    //   var array = [1,2,3,4,5];
    //   var sum = 0;
    //   array.forEach((element) => {
    //     sum += element;
    //   });
    //   expect(
    //     sum
    //   ).to.eql(
    //     15
    //   );
    //   /* #@range_end(forEach_in_array) */
    //   next();
    // });
      expect(
        func.hasOwnProperty('name')
      ).to.eql(
        true
      );
      var id = (any) => {
        return any;
      };
      expect(
        ((x) => {
          return x;
        })(3)
      ).to.be(
        3
      );
    it('関数はデータと類似している', (next) => {
      // ~~~scala
      // trait env extends terms {
      //   abstract class Env {
      //     def apply[a](v : Var[a]): a
      //     def extend[a](v : Var[a], x : a) = new Env {
      //       def apply[b](w: Var[b]): b = w match {
      //         case _: v.type => x
      //         case _ =>  Env.this.apply(w)
      //       }}}
      //   object emptyEnv extends Env {
      //     def apply[a](x : Var[a]): a = throw new Error("not found : "+x.name)
      //   }
      // }
      // ~~~
      /* #@range_begin(function_as_object) */
      var empty = (key) => {
        return null;
      };
      var get = (key, obj) => {
        return obj(key);
      };
      var set = (key, value, obj) => {
        return (key2) => {
          if(key === key2) {
            return value;
          } else {
            return get(key2,obj);
          }
        };
      };
      expect(
        get(1,set(1,"one",empty)) // {1: "one"}[1] => "one"
      ).to.eql(
        "one"
      );
      expect(
        get(2,set(2,"two",set(1,"one",empty)))  // {1: "one", 2: "two"}[2] => "two"
      ).to.eql(
        "two"
      );
      expect(
        get(1,set(2,"two",set(1,"one",empty)))  // {1: "one", 2: "two"}[1] => "one"
      ).to.eql(
        "one"
      );
      /* #@range_end(function_as_object) */
      next();
    });
    it('環境の実装', (next) => {
      /* #@range_begin(environment_implemented) */
      /* 空の環境 */
      var emptyEnv = (variable) => {
        return null;
      };
      /* 変数名に対応する値を環境から取りだす */
      var lookupEnv = (variable, env) => {
        return env(variable);
      };
      /* 環境を拡張する */
      var extendEnv = (variable, value, env) => {
        return (queryVariable) => {
          if(variable === queryVariable) {
            return value;
          } else {
            return lookupEnv(queryVariable,env);
          }
        };
      };
      /* #@range_end(environment_implemented) */
      /* #@range_begin(environment_implemented_usage) */
      expect(((_) => {
        /* 空の辞書を作成する */
        var initEnv = emptyEnv;
        /* var a = 1 を実行して、辞書を拡張する */
        var firstEnv = extendEnv("a", 1, initEnv);
        /* var b = 3 を実行して、辞書を拡張する */
        var secondEnv = extendEnv("b",3, firstEnv);
        /* 辞書から b の値を参照する */
        return lookupEnv("b",secondEnv);
      })()).to.eql(
        3
      );
      /* #@range_end(environment_implemented_usage) */
      /* #@range_begin(environment_implemented_shadowing_usage) */
      expect(((_) => {
        /* 空の辞書を作成する */
        var initEnv = emptyEnv;
        /* var x = 1 を実行して、辞書を拡張する */
        var xEnv = extendEnv("x", 1, initEnv);
        /* var z = 2 を実行して、辞書を拡張する */
        var zEnv = extendEnv("z", 2, xEnv);
        /* 内部のスコープで var x = 3 を実行して、辞書を拡張する */
        var xEnvInner = extendEnv("x",3, zEnv);
        /* 内部のスコープで var y = 4 を実行して、辞書を拡張する */
        var innerMostEnv = extendEnv("y",4, xEnvInner);
        /* 一番内側のスコープを利用して x + y + z を計算する */
        return lookupEnv("x",innerMostEnv) + lookupEnv("y",innerMostEnv) + lookupEnv("z",innerMostEnv) ;
      })()).to.eql(
        3 + 4 + 2
      );
      /* #@range_end(environment_implemented_shadowing_usage) */
      next();
    });
  describe('記憶域と参照', () => {
    it('変数は参照を保持する', (next) => {
      /* #@range_begin(variables_hold_reference) */
      expect((() => {
        var x = 1;
        var y = x;
        y = 2;
        return x;
      })()).to.be(
        1
      );
      expect((() => {
        var x = 1;
        var y = x;
        x = 2;
        return y;
      })()).to.be(
        1
      );
      expect((() => {
        var x = [0,1,2,3];
        var y = x;
        return x === y;
      })()).to.be(
        true
      );
      /* #@range_end(variables_hold_reference) */
      next();
    });
  });
    it('代入によって参照透過性を失う場合', (next) => {
      /* #@range_begin(referential_transparency_counterexample_assignment) */
      var x = 1;
      var y = 2;
      var z = x + y;
      x = 3;
      expect(
        (x + y) * (x + y)
      ).not.to.be(
        z * z // (x + y) を z に置きかえた
      );
      /* #@range_end(referential_transparency_counterexample_assignment) */
      next();
    });
