"use strict";

// Copyright 2015 John Clements <clements@brinckerhoff.org>
// this file contains the code for the password generator;
// all of the jquery should be in this file, making the
// library more unit-testable.

var GeneratePassword =  (function() {

  function bitlistToNibble(bitlist, idx, accum){
    if (bitlist.length === idx) {
      return accum;
    } else {
      var shifted = accum << 1;
      var newAccum = (accum << 1) + (bitlist[idx] ? 1 : 0);
      return bitlistToNibble(bitlist, idx+1, newAccum);
    }
  }

  function bitlistToHexString(bitlist){
    var remainder = (bitlist.length % 4);
    var nibbles = (bitlist.length - remainder) / 4;
    var str = (remainder === 0
               ? ""
               : (bitlistToNibble(bitlist.slice(0,remainder),0,0)
                  .toString(16)));
    for (var i = 0; i<nibbles; i++) {
      var nextBits = bitlist.slice(remainder+(i*4),remainder+((i+1)*4));
      str = str + bitlistToNibble(nextBits,0,0).toString(16);
    }
    return str;
  }

  var DEFAULTBITS = 56;

  // old browser compatibility, grr:
  var myIsNaN = (Number.isNaN ? Number.isNaN
               : function(n){return n !== n;});
  
  // gets the current number of bits from the box.
  // EFFECT: sets it to the calculated value.
  function getNumBits(){
    var numBitsString = $('#numBits')[0].value;
    var numBitsParsed = parseInt(numBitsString,10);
    var numBits = ((myIsNaN(numBitsParsed)
                   || (numBitsParsed < 0)
                   || (numBitsParsed > MAXPASSWORDBITS))
                   ? (numBitsAlert(numBitsString),
                      DEFAULTBITS)
                   : numBitsParsed);
    $('#numBits').val(numBits.toString(10));
    return numBits;
  }

  // put up an alert box about an unparseable number
  function numBitsAlert(str){
    alert("couldn't parse string \""+str+"\" as a number in [0..500]");
  }

  // given a list of booleans, return a password
  function bitlistToPassword(bitlist,seed){
    var seq = MolisHai.generateSequence(SeedModel,bitlist,TextModel);
    // can't do this without ES6:
    // var fst = (a)=>(a[0]);
    var fst = function(a){return a[0];};
    // can't do this without ES6:
    //var concat = (a,b)=>(a.concat(b));
    var concat = function(a,b){return a.concat(b);};
    return _.foldl(_.map(seq,fst),concat);
  }

  var DEFAULTBITS = 56;
  var MAXPASSWORDBITS = 500;

  // generate a bitlist and password
  function bitsAndPword(){
    var numBits = getNumBits();
    var bits = MolisHai.getBits(numBits);
    var pword = bitlistToPassword(bits);
    return {bits:bits,pword:pword};
  }

  // generate a table row
  function tableRow(){
    // can't do this without ES6:
    //var {bits:bits,pword:pword} = bitsAndPword();
    // awkward old way to write it:
    var bap = bitsAndPword();
    var bits = bap.bits;
    var pword = bap.pword;
    var asbits = bitlistToHexString(bits);
    // appalling... is there a structured way to do this?
    return '<tr><td>'+asbits+'</td><td class="passwordtext">'+pword+'</td></tr>'
  }

  function regenerate(){
    var numBits = getNumBits();
    var bits = MolisHai.getBits(numBits);
    // quoting here?
    $('#passwordtable').html(
        '<tr><th>bits</th><th>password</th></tr>'
      // nasty, fix this in a second:
      + tableRow()
      + tableRow()
      + tableRow()
      + tableRow()
      + tableRow()
      + tableRow()
      + tableRow()
      + tableRow()
    )
  }

  return {
    start : (function(){
      regenerate();
    }),
    gobuttonclick : (function(){
      regenerate();
    })}

  // TESTS

    QUnit.test("conversions", function(assert){
    assert.strictEqual(bitlistToNibble([false, true, false],0,0),2);
    assert.strictEqual(bitlistToNibble([true,false, true, false],0,0),10);
  })

  QUnit.test("multi-nibble conversions",function(assert){
    assert.strictEqual(bitlistToHexString([false,true,false]),"2");
    assert.strictEqual(bitlistToHexString([false,true,false,true]),"5");
    assert.strictEqual(bitlistToHexString([false,true,false,true,true]),"0B");
    assert.strictEqual(bitlistToHexString(MolisHai.getBits(56)).length,14);

  })

  QUnit.test("bitlist->password",function(assert){
    assert.strictEqual(bitlistToPassword([true,true,false,true]),"av");
  })


}());

$( document ).ready(function() {
  //GeneratePassword.start();
  // set up a handler for the go button
  $( "#gobutton" ).click(function( event ) {
    GeneratePassword.gobuttonclick();
    event.preventDefault();
    });
  // generate passwords just to have some there:
  GeneratePassword.start();
});
