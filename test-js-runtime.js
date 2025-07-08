// Test the PathFinder JavaScript Runtime
const PF = {
  // Natural numbers - use BigInt for arbitrary precision
  nat: (n) => ({ tag: 'nat', value: BigInt(n) }),
  zero: { tag: 'nat', value: 0n },
  succ: (n) => ({ tag: 'nat', value: n.value + 1n }),
  
  // Natural elimination
  natElim: (motive, base, step, n) => {
    if (n.value === 0n) return base;
    let acc = base;
    for (let i = 0n; i < n.value; i++) {
      acc = step({ tag: 'nat', value: i })(acc);
    }
    return acc;
  },
  
  // String conversion
  toString: (val) => {
    switch (val.tag) {
      case 'nat': return val.value.toString();
      default: return JSON.stringify(val);
    }
  }
};

// Test addition using nat-elim
const add = (x) => (y) => {
  return PF.natElim(
    null,  // motive (ignored)
    x,     // base case: x + 0 = x
    (n) => (rec) => PF.succ(rec),  // step: x + (n+1) = succ(x + n)
    y      // eliminate on y
  );
};

// Create some numbers
const two = PF.succ(PF.succ(PF.zero));
const three = PF.succ(PF.succ(PF.succ(PF.zero)));

// Test addition
const five = add(two)(three);

console.log("Testing PathFinder JS Runtime:");
console.log("two =", PF.toString(two));
console.log("three =", PF.toString(three));
console.log("two + three =", PF.toString(five));
console.log("Expected: 5");
console.log("Success:", five.value === 5n);