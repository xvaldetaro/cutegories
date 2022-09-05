
export function copyUrlToClipboard() {
  const dummy = document.createElement('input');
  const text = window.location.href;

  document.body.appendChild(dummy);
  dummy.value = text;
  dummy.select();
  document.execCommand('copy');
  document.body.removeChild(dummy);
}