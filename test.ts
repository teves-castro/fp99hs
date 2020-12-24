function myIf(p: Boolean, tv: any, fv: any) {
  if (p) {
    return tv;
  } else {
    return fv;
  }
}

const error = () => {
  throw new Error("X");
};

console.log(myIf(false, error(), "hello"));
