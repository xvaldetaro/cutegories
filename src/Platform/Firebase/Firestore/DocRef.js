// Initialize Cloud Firestore and get a reference to the service
export const id_ = (ref) => ref.id

export const path_ = (ref) => ref.path

export const parent_ = (ref) => (just) => (nothing) => {
  if (!ref.parent || !ref.parent.parent || ref.parent.parent.type != "document") {
    return nothing;
  }
  return (just(ref.parent.parent));
}