"use strict";

// Copyright 2015 John Clements <clements@brinckerhoff.org>
// this library turns sequences of booleans (that is,
// bit strings) into passwords, using a given text model.
// It requires a single huffman tree that's used to generate
// the initial markov model state, and a mapping from
// states to huffman trees of 1-char strings, used to advance
// to the next state. Let's do this like we would in HtDP...

// The model is parameterized by an "order"; the markov states
// are strings of length "order".

// Right now, we're using order 2.


// A MarkovState is a string of length 'order'.

// a SeedModel is a HuffmanTree where the leaves are MarkovStates.

// a TextModel is a mapping from MarkovStates to TransitionTrees.

// a TransitionTree is a HuffmanTree where the leaves are
//   single characters, a.k.a. strings of length 1.

// A HuffmanTree is one of
// - a string, representing a leaf, or
// - an object containing fields 'a' and 'b', representing
//    a branch (or 'node', if you prefer).

// really, they're just binary trees. I call them huffman
// trees because that's how they're being used.


var MolisHai = (function() {

  // randomly generated string: "There twass molis hai"

  // a char-tree is either a string of length one,
  // or an object containing two fields: a and b,
  // where both are char-trees
  var tree = {a:'a',b:'b'};

  // ah, JavaScript...
  if ((typeof window === "undefined")
    || (typeof window.crypto === "undefined")
    || (typeof window.crypto.getRandomValues === "undefined")) {
    alert("can't find window.crypto.getRandomValues. Can't generate secure passwords.");
    // fall back to insecure random number generation here?
  }

  /* assuming that window.crypto.getRandomValues is available */

  // convert a uint16 to a list of bits
  function uint16ToBitList(x){
    var mask = 0x8000;
    var result = [];
    var i;
    for (i=0;i<16;i++) {
      result[i] = ((x & mask) !== 0);
      mask = mask >> 1;
    }
    return result;
  }


  // convert a Uint16Array to a list of bits
  function arrToBitList(arr){
    return (_.flatten(_.map(arr,uint16ToBitList)));
  }

  // walk down a tree until reaching a leaf. Return
  // the leaf and the remaining bits
  function pickLeaf(tree,bitsource){
    // is it a branch?
    if (tree.hasOwnProperty('a')) {
      if (bitsource.length === 0){
        return pickLeaf(tree.a,bitsource);
      } else if (bitsource[0]) {
        return pickLeaf(tree.a,bitsource.slice(1));
      } else {
        return pickLeaf(tree.b,bitsource.slice(1));
      }
    } else {
      return {leaf:tree,remaining:bitsource};
    }
  }

  // given a seed (markov cell) and a list of booleans and a hash
  // of huffman trees, generate a sequence of (cons char number),
  // where the number indicates how many bits of entropy were used for
  // each character.
  function generateSequenceWithSeed(seed,bools,treeMap){
    if (bools.length === 0) {
      return [];
    } else {
      // can't do this until ES6:
      // var {leaf:next,remaining:remaining} = pickLeaf(treeMap[seed],bools);
      // awkward old way:
      var leafAndRemaining = pickLeaf(treeMap[seed],bools);
      var next = leafAndRemaining.leaf;
      var remaining = leafAndRemaining.remaining;
      return [[next,(bools.length - remaining.length)]]
        .concat(generateSequenceWithSeed(seed.slice(1)+next,remaining,treeMap));
    }
  }

  // given a seed tree and a list of booleans and a hash of huffman
  // trees, generate a sequence of (cons char number), where
  // the number indicates how many bits of entropy were used
  // for each character.
  function generateSequence(seedTree,bools,treeMap){
    if (bools.length === 0){
      return [];
    } else {
      // can't do this without ES6:
      // var {leaf:seed,remaining:remaining} = pickLeaf(seedTree,bools);
      // old way:
      var lar = pickLeaf(seedTree,bools);
      var seed = lar.leaf;
      var remaining = lar.remaining;
      return [[seed,(bools.length - remaining.length)]]
        .concat(generateSequenceWithSeed(seed,remaining,treeMap));
    }
  }

  //
  //
  // EXTERNAL FUNCTIONS:
  //
  //

  // is this a legal seed (that is, one that appears in the hash)?
  function isLegalSeed(seed,treeMap){
    return treeMap.hasOwnProperty(seed);
  }
  
  // get a new sequence of 'n' bits.
  function getBits(n){
    var arr = new Uint16Array(Math.ceil(n/16));
    window.crypto.getRandomValues(arr);
    return arrToBitList(arr).slice(0,n);
  }

  // TESTS:

  // I bet ES6 has a nice way to manage the tests...


  QUnit.test("uint16ToBitList tests",function(assert){
    assert.deepEqual(uint16ToBitList(24),
                       [false, false, false, false,
                        false, false, false, false,
                        false, false, false, true,
                        true,  false, false, false])
  })

  QUnit.test("arrToBitList tests",function(assert){
    assert.deepEqual(arrToBitList(new Uint16Array([24,13])),
                       [false, false, false, false,
                        false, false, false, false,
                        false, false, false, true,
                        true,  false, false, false,
                        false, false, false, false,
                        false, false, false, false,
                        false, false, false, false,
                        true, true, false, true]);})


  QUnit.test("traverse tests", function (assert){
    var testTree = {a:3,b:{a:{a:1,b:2},b:4}}
    function runTest(bits,result) {
      assert.deepEqual(pickLeaf(testTree,bits),result);
    }
    runTest([],{leaf:3,remaining:[]});
    runTest([true],{leaf:3,remaining:[]});
    runTest([true,true],{leaf:3,remaining:[true]});
    runTest([false,true],{leaf:1,remaining:[]});
    runTest([false,false],{leaf:4,remaining:[]});
  });

  QUnit.test("TextModel test", function(assert){
    assert.deepEqual(pickLeaf(TextModel["xp"],[false,true,false,false]),
                     {leaf:"l",remaining:[false]})
  });

  QUnit.test("SeedModel test", function(assert){
    assert.deepEqual(pickLeaf(SeedModel,[true,true,false,true]),
                    {leaf:" i",remaining: []})
  });

  QUnit.test("generateSequenceWithSeed test", function(assert){
    assert.deepEqual(generateSequenceWithSeed("re",[true,false,true,true,
                                            false,true,false],
                                      TextModel),
                     [["a",3],["s",3],[" ",1]])});

    QUnit.test("generateSequence test", function(assert){
      assert.deepEqual(generateSequence(SeedModel,[true, true, false, true,
                                                   true,false,true,true,
                                                   false,true,false],
                                        TextModel),
                       // regression only...
                       [[" i",4],["t",2],["e",4],[".",1]])})

  // Return the results of applying the iteratee to each element.
  function andmap(obj, iteratee, context) {
    var length = obj.length,
        result = true;
    for (var index = 0; index < length; index++) {
      if (!(iteratee(obj[index]))) {
        return false;
      }
    }
    return true;
  };

  QUnit.test("getBits length tests", function(assert){
    assert.strictEqual(getBits(32).length,32);
    assert.strictEqual(getBits(33).length,33);
    assert.strictEqual(getBits(7).length,7);
    assert.ok(andmap(getBits(6),function(a){return ((a===true)||(a===false));}));
  })

  QUnit.test("legalseed tests", function(assert){
    assert.strictEqual(isLegalSeed("ab",{}),false);
    assert.strictEqual(isLegalSeed("ab",TextModel),true);
    assert.strictEqual(isLegalSeed("XQ",TextModel),false);
  })
  return {
    getBits : getBits,
    generateSequence : generateSequence,
    isLegalSeed : isLegalSeed
  };
}());
