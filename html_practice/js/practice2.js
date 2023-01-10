//. Modeuls
const csv = require("jquery-csv");
const fs = require('fs');

//.You can use Var, Const, or Let, use Let for browsers before 2015
var trace1 = {
    x: [1, 2, 3, 4],
    y: [10, 15, 13, 55],
    type: 'scatter'
  };
  
  var trace2 = {
    x: [1, 2, 3, 4],
    y: [16, 5, 11, 150],
    type: 'scatter'
  };
  
  var data = [trace1, trace2];

  const array_test = ["array",1,3,4,5]

  console.log(array_test[0])

  $.csv.toArrays("macro_dat.csv")