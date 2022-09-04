var stringSimilarity = require("string-similarity");

var similarity = stringSimilarity.compareTwoStrings("healed", "sealed");

var matches = stringSimilarity.findBestMatch("fazendo", [
  "fazer",
  "bunda",
  "theatre",
]);

console.log(matches)