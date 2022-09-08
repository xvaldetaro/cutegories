import stringSimilarity from "string-similarity";

export const findBestMatch = (str) => (others) => {
  const ratings = stringSimilarity.findBestMatch(str, others).ratings.filter((a) => {
    return str != a.target
  })

  ratings.sort((a, b) => {
    if (a.rating < b.rating) {
      return 1;
    } else if (a.rating > b.rating) {
      return -1;
    } else {
      return 0;
    }
  });
  return ratings;
}

export const randomLetter = (exclude) => () => {
  const dict = {};
  exclude.forEach((l) => {
    dict[l] = true
  });
  let base = Math.floor(Math.random() * 26)
  let str = String.fromCharCode(97 + base);
  while (dict[str]) {
    base = (base + 1) % 26
    str = String.fromCharCode(97 + base);
  }
  return str;
}