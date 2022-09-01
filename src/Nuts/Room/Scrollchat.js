
export function scrollChat() {
  const chatWrapper = document.querySelector('#chatbox');
  console.log("height: ", chatWrapper.offsetHeight );
  chatWrapper.scrollTo(0, chatWrapper.offsetHeight );
}