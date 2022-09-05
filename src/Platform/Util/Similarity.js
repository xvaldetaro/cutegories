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

export const randomLetter = () => String.fromCharCode(97+Math.floor(Math.random() * 26))