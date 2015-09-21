"use strict";

var expect = require('expect.js');

describe('高階関数', () => {
  describe('コンビネーター', () => {
    it('コンビネーター・ライブラリー', (next) => {
      /* #@range_begin(combinator_library) */
	  var id = (any) => {
		return any;
	  };
	  var get = (key) => {
		  return (obj) => {
			return obj[key];
		  };
	  };
	  var isEqual = (n1) => {
		return (n2) => {
		  return n2 === n1;
		};
	  };
	  var isLessThan = (n1) => {
		return (n2) => {
		  return n1 > n2;
		};
	  };
	  var isMoreThan = (n1) => {
		return (n2) => {
		  return n1 < n2;
		};
	  };
	  var not = (predicate) => {
		return (data) => {
		  return ! predicate(data);
		}
	  };
	  var within = (lower) => {
		return (upper) => {
		  return (data) => {
			return (extractor) => {
			  return and(extractor, isMoreThan(lower))(extractor, isLessThan(upper))(data);
			};
		  };
		};
	  };
	  var and = (firstExtractor, firstPredicate) => {
		return (nextExtractor, nextPredicate) => {
		  return (data) => {
			var firstResult = firstPredicate(firstExtractor(data))
			if(! firstResult) {
			  return false;
			} else {
			  return nextPredicate(nextExtractor(data));
			}
		  }
		};
	  };
	  var or = (firstExtractor, firstPredicate) => {
		return (nextExtractor, nextPredicate) => {
		  return (data) => {
			var firstResult = firstPredicate(firstExtractor(data))
			if(firstResult) {
			  return true;
			} else {
			  return nextPredicate(nextExtractor(data));
			}
		  }
		};
	  };
      /* #@range_end(combinator_library) */
      /* #@range_begin(combinator_library_test) */
  	  var data = {
  		temp: 24,
  		time: new Date("2013/2/15 17:57:27")
  	  };
	  expect(((_) => {
		var getTemp = (data) => {
		  return get('temp')(data);
		};
		var getHour = (data) => {
		  return get('time')(data).getHours();
		};
		return and(getTemp, isMoreThan(20))(getHour, isEqual(17))(data)
	  })()).to.eql(
  		true
  	  )
	  expect(((_) => {
		var getTemp = (data) => {
		  return get('temp')(data);
		};
		var getHour = (data) => {
		  return get('time')(data).getHours();
		};
		return or(getTemp, isMoreThan(30))(getHour, isEqual(17))(data)
	  })()).to.eql(
  		true
  	  )
	  expect(
		within(20)(30)(get('temp')(data))(id)
	  ).to.eql(
  		true
  	  )
	  expect(
		within(20)(30)(data)(get('temp'))
	  ).to.eql(
  		true
  	  )
	  expect(
		within(20)(30)(data)((data) => {
		  return get('temp')(data)
		})
	  ).to.eql(
  		true
  	  )
      /* #@range_end(combinator_library_test) */
	  next();
	});
  });
});
