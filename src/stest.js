var stringSimilarity = require("string-similarity");

var similarity = stringSimilarity.compareTwoStrings("healed", "sealed");

var matches = stringSimilarity.findBestMatch("healed", [
  "edward",
  "sealed",
  "theatre",
]);

console.log(matches)