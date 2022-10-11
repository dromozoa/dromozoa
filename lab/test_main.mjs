import chunk from "./test_module.mjs";

chunk();

class C1 extends Function {
  constructor(fn) {
    super();
    this.fn = fn;
  }

  call(...args) {
    this.fn(...args);
  }
}

const c1 = new C1(x => console.log(x));
c1.call("test");
