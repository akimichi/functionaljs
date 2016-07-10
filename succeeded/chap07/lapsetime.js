"use strict";

var startUpTime = Date.now();

var application = {
  timeLapse: () => {
    var now = Date.now();
    return now - startUpTime; // 外側のスコープにある変数startUpTimeを参照している
  }
};

console.log(application.timeLapse());
