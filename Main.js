// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            if(t.x === __updatable) {
                t.x = f();
            } else {
                return f();
            }
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;
    case 'wheel':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            var mdx = [0,x.deltaX];
            var mdy = [0,x.deltaY];
            var mdz = [0,x.deltaZ];
            B(A(cb,[[0,mx,my],[0,mdx,mdy,mdz],0]));
        };
        break;
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsElemsByClassName(cls) {
    var es = document.getElementsByClassName(cls);
    var els = [0];

    for (var i = es.length-1; i >= 0; --i) {
        els = [1, [0, es[i]], els];
    }
    return els;
}

function jsQuerySelectorAll(elem, query) {
    var els = [0], nl;

    if (!elem || typeof elem.querySelectorAll !== 'function') {
        return els;
    }

    nl = elem.querySelectorAll(query);

    for (var i = nl.length-1; i >= 0; --i) {
        els = [1, [0, nl[i]], els];
    }

    return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, jsRead(obj)];
    case 'string':
        return [1, obj];
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);}),true]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}
window['arr2lst'] = arr2lst;

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}
window['lst2arr'] = lst2arr;

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
    var n = s.length,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=s.length; i+=64) {
        md5cycle(state, md5blk(s.substring(i-64, i)));
    }
    s = s.substring(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<s.length; i++)
        tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s.charCodeAt(i)
            + (s.charCodeAt(i+1) << 8)
            + (s.charCodeAt(i+2) << 16)
            + (s.charCodeAt(i+3) << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s) {
    return hex(md51(s));
}

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return [0, 1, E(w).val];
}

function finalizeWeak(w) {
    return [0, B(A(E(w).fin, [0]))];
}

var _0=function(_1,_2,_){var _3=jsCreateTextNode(toJSStr(E(_1))),_4=_3,_5=jsAppendChild(_4,E(_2)[1]);return [0,_4];},_6=function(_7){var _8=B(A(_7,[_])),_9=_8;return E(_9);},_a=function(_b){return new F(function(){return _6(function(_){var _=0;return new F(function(){return eval(_b);});});});},_c=0,_d=function(_e,_f,_g,_h){return new F(function(){return A(_e,[function(_){var _i=jsSetAttr(E(_f)[1],toJSStr(E(_g)),toJSStr(E(_h)));return _c;}]);});},_j=new T(function(){return B(unCStr("stylesheet"));}),_k=new T(function(){return B(unCStr("rel"));}),_l=new T(function(){return B(unCStr("(function(){return document.head;})"));}),_m=function(_n,_){var _o=B(A(_a,[toJSStr(E(_l)),_])),_p=_o,_q=B(A(_n,[[0,_p],_])),_r=_q;return _c;},_s=new T(function(){return B(unCStr("href"));}),_t=function(_u){return E(_u);},_v=new T(function(){return B(unCStr("link"));}),_w=function(_x,_){var _y=jsCreateElem(toJSStr(E(_v))),_z=_y,_A=jsAppendChild(_z,E(_x)[1]);return [0,_z];},_B=function(_C,_){return new F(function(){return _m(function(_D,_){var _E=B(_w(_D,_)),_F=_E,_G=B(A(_d,[_t,_F,_k,_j,_])),_H=_G,_I=B(A(_d,[_t,_F,_s,_C,_])),_J=_I;return _F;},_);});},_K=function(_L,_){return _L;},_M=new T(function(){return B(unCStr("script"));}),_N=function(_O,_P,_Q,_){var _R=jsCreateElem(toJSStr(E(_M))),_S=_R,_T=jsAppendChild(_S,E(_Q)[1]),_U=[0,_S],_V=B(A(_O,[_P,_U,_])),_W=_V;return _U;},_X=new T(function(){return B(unCStr("src"));}),_Y=function(_Z){return E(_Z);},_10=function(_11,_){return new F(function(){return _m(function(_12,_){var _13=B(_N(_Y,_K,_12,_)),_14=_13,_15=B(A(_d,[_t,_14,_X,_11,_])),_16=_15;return _14;},_);});},_17=new T(function(){return B(unCStr("style"));}),_18=new T(function(){return B(unCStr("./bootstrap-theme.min.css"));}),_19=new T(function(){return B(unCStr("./bootstrap.min.css"));}),_1a=2,_1b=new T(function(){return B(unCStr("span"));}),_1c=new T(function(){return [0,"arr2lst"];}),_1d=function(_1e,_1f){return new F(function(){return _6(function(_){var _=0;return new F(function(){return A(_a,[E(_1c)[1],E(_1e),E(_1f),_]);});});});},_1g=[0],_1h=new T(function(){return B(_a("(function(sel){return document.querySelectorAll(sel);})"));}),_1i=function(_1j,_1k,_1l,_){var _1m=B(A(_1h,[E(toJSStr(E(_1j))),_])),_1n=_1m,_1o=function(_1p,_){var _1q=E(_1p);if(!_1q[0]){return _1g;}else{var _1r=B(A(_1k,[[0,_1q[1]],_])),_1s=_1r,_1t=B(_1o(_1q[2],_)),_1u=_1t;return [1,_1s,_1u];}},_1v=B(_1o(B(_1d(_1n,0)),_)),_1w=_1v;return _1l;},_1x=new T(function(){return B(unCStr("Prelude.undefined"));}),_1y=new T(function(){return B(err(_1x));}),_1z=function(_1A,_1B,_1C,_1D,_){var _1E=B(A(_1C,[_1D,_])),_1F=_1E,_1G=E(_1F),_1H=E(_1G[1]),_1I=_1H[1];return [0,[0,function(_1J,_){switch(E(_1B)){case 0:var _1K=B(_1i(_1A,_1I,_1y,_)),_1L=_1K;return _1J;case 1:var _1M=B(_1i(_1A,function(_1N,_){var _1O=E(_1N),_1P=_1O[1],_1Q=jsGetChildren(_1P),_1R=_1Q,_1S=E(_1R);if(!_1S[0]){var _1T=B(A(_1I,[_1O,_])),_1U=_1T;return _1O;}else{var _1V=jsCreateElem(toJSStr(E(_1b))),_1W=_1V,_1X=jsAddChildBefore(_1W,_1P,E(_1S[1])[1]),_1Y=B(A(_1I,[[0,_1W],_])),_1Z=_1Y;return _1O;}},_1y,_)),_20=_1M;return _1J;default:var _21=B(_1i(_1A,function(_22,_){var _23=E(_22),_24=jsClearChildren(_23[1]),_25=B(A(_1I,[_23,_])),_26=_25;return _23;},_1y,_)),_27=_21;return _1J;}},_1H[2]],_1G[2]];},_28=[0,5],_29=[0,2],_2a=[0,-10],_2b=[1,_2a,_1g],_2c=[0,-15],_2d=[1,_2c,_2b],_2e=[0,-5],_2f=[1,_2e,_2d],_2g=[0,-25],_2h=[1,_2g,_2f],_2i=[1,_2a,_2h],_2j=[1,_2c,_2i],_2k=[0,-40],_2l=[1,_2k,_2j],_2m=[1,_2g,_2l],_2n=[0,-30],_2o=[1,_2n,_2m],_2p=[1,_2a,_2o],_2q=[1,_2e,_2p],_2r=[0,-1000],_2s=[1,_2r,_2q],_2t=[1,_2c,_1g],_2u=[1,_2g,_2t],_2v=[1,_2a,_2u],_2w=[1,_2c,_2v],_2x=[1,_2e,_2w],_2y=[1,_2n,_2x],_2z=[0,-20],_2A=[1,_2z,_2y],_2B=[0,-18],_2C=[1,_2B,_2A],_2D=[1,_2k,_2C],_2E=[1,_2z,_2D],_2F=[1,_2r,_2E],_2G=[1,_2e,_2F],_2H=[1,_2k,_1g],_2I=[1,_2z,_2H],_2J=[1,_2k,_2I],_2K=[1,_2e,_2J],_2L=[1,_2c,_2K],_2M=[1,_2e,_2L],_2N=[1,_2c,_2M],_2O=[1,_2k,_2N],_2P=[1,_2c,_2O],_2Q=[1,_2r,_2P],_2R=[1,_2z,_2Q],_2S=[1,_2a,_2R],_2T=[1,_2n,_1g],_2U=[1,_2e,_2T],_2V=[1,_2g,_2U],_2W=[1,_2a,_2V],_2X=[0,-50],_2Y=[1,_2X,_2W],_2Z=[1,_2g,_2Y],_30=[0,-35],_31=[1,_30,_2Z],_32=[1,_2c,_31],_33=[1,_2r,_32],_34=[1,_2c,_33],_35=[1,_2k,_34],_36=[1,_2n,_35],_37=[1,_2g,_1g],_38=[1,_2a,_37],_39=[1,_2X,_38],_3a=[1,_2c,_39],_3b=[1,_2z,_3a],_3c=[1,_2a,_3b],_3d=[1,_2g,_3c],_3e=[1,_2r,_3d],_3f=[1,_2c,_3e],_3g=[1,_2k,_3f],_3h=[1,_2B,_3g],_3i=[1,_2g,_3h],_3j=[1,_2r,_1g],_3k=[1,_2e,_3j],_3l=[1,_2z,_3k],_3m=[1,_2B,_3l],_3n=[1,_2e,_3m],_3o=[1,_2e,_3n],_3p=[1,_30,_3o],_3q=[1,_2g,_3p],_3r=[1,_2n,_3q],_3s=[1,_2k,_3r],_3t=[1,_2c,_3s],_3u=[1,_2a,_3t],_3v=[1,_3u,_1g],_3w=[1,_2e,_1g],_3x=[1,_2r,_3w],_3y=[1,_2z,_3x],_3z=[1,_2a,_3y],_3A=[1,_2k,_3z],_3B=[1,_2n,_3A],_3C=[1,_2e,_3B],_3D=[1,_2a,_3C],_3E=[1,_2e,_3D],_3F=[1,_2z,_3E],_3G=[1,_2g,_3F],_3H=[1,_2c,_3G],_3I=[1,_3H,_3v],_3J=[1,_2z,_1g],_3K=[1,_2z,_3J],_3L=[1,_2r,_3K],_3M=[1,_2c,_3L],_3N=[1,_2n,_3M],_3O=[1,_2c,_3N],_3P=[0,-70],_3Q=[1,_3P,_3O],_3R=[1,_2X,_3Q],_3S=[1,_2g,_3R],_3T=[1,_2k,_3S],_3U=[1,_2a,_3T],_3V=[1,_2e,_3U],_3W=[1,_3V,_3I],_3X=[1,_2B,_1g],_3Y=[1,_2a,_3X],_3Z=[1,_2c,_3Y],_40=[1,_2r,_3Z],_41=[1,_2g,_40],_42=[1,_2z,_41],_43=[1,_2n,_42],_44=[1,_2c,_43],_45=[1,_2a,_44],_46=[1,_2e,_45],_47=[1,_2c,_46],_48=[1,_2g,_47],_49=[1,_48,_3W],_4a=[1,_2k,_3w],_4b=[1,_2n,_4a],_4c=[1,_2g,_4b],_4d=[1,_2r,_4c],_4e=[1,_2a,_4d],_4f=[1,_2n,_4e],_4g=[1,_2z,_4f],_4h=[1,_2X,_4g],_4i=[1,_2c,_4h],_4j=[1,_2e,_4i],_4k=[1,_2a,_4j],_4l=[1,_4k,_49],_4m=[1,_2n,_3w],_4n=[1,_2c,_4m],_4o=[1,_2z,_4n],_4p=[1,_2a,_4o],_4q=[1,_2r,_4p],_4r=[1,_2e,_4q],_4s=[1,_2a,_4r],_4t=[1,_2g,_4s],_4u=[1,_2e,_4t],_4v=[1,_2n,_4u],_4w=[1,_2c,_4v],_4x=[1,_4w,_4l],_4y=[1,_30,_1g],_4z=[1,_2e,_4y],_4A=[1,_3P,_4z],_4B=[1,_2n,_4A],_4C=[1,_2n,_4B],_4D=[1,_2e,_4C],_4E=[1,_2r,_4D],_4F=[1,_2g,_4E],_4G=[1,_30,_4F],_4H=[1,_2c,_4G],_4I=[1,_2z,_4H],_4J=[1,_2k,_4I],_4K=[1,_4J,_4x],_4L=[1,_3i,_4K],_4M=[1,_36,_4L],_4N=[1,_2S,_4M],_4O=[1,_2G,_4N],_4P=[1,_2s,_4O],_4Q=[0,500],_4R=[1,_4Q],_4S=[0,50],_4T=[0,1],_4U=[0,500],_4V=[0,0.1],_4W=[0,0.8],_4X=[0,_4W,_4V,_4U,_4T,_4S,_4R],_4Y=[0,_4P,_29,_28,_4X],_4Z=[0,_4Y],_50=new T(function(){return B(unCStr("color:red"));}),_51=new T(function(){return B(unCStr("style"));}),_52=[0,_51,_50],_53=[1,_52,_1g],_54=[0,98],_55=[1,_54,_1g],_56=function(_57,_58,_59,_){var _5a=E(_58),_5b=B(A(_57,[_59,_])),_5c=_5b,_5d=B(A(_d,[_t,_5c,_5a[1],_5a[2],_])),_5e=_5d;return _5c;},_5f=function(_5g,_5h){while(1){var _5i=(function(_5j,_5k){var _5l=E(_5k);if(!_5l[0]){return E(_5j);}else{_5g=function(_5m,_){return new F(function(){return _56(_5j,_5l[1],_5m,_);});};_5h=_5l[2];return null;}})(_5g,_5h);if(_5i!=null){return _5i;}}},_5n=function(_5o,_5p,_){var _5q=jsCreateElem(toJSStr(E(_5o))),_5r=_5q,_5s=jsAppendChild(_5r,E(_5p)[1]);return [0,_5r];},_5t=function(_5u){return new F(function(){return _5f(function(_5v,_){var _5w=B(_5n(_55,_5v,_)),_5x=_5w,_5y=B(A(_5u,[_5x,_])),_5z=_5y;return _5x;},_53);});},_5A=[0],_5B=function(_5C,_5D){var _5E=E(_5C);return _5E[0]==0?E(_5D):[1,_5E[1],new T(function(){return B(_5B(_5E[2],_5D));})];},_5F=function(_5G,_5H){var _5I=jsShowI(_5G),_5J=_5I;return new F(function(){return _5B(fromJSStr(_5J),_5H);});},_5K=[0,41],_5L=[0,40],_5M=function(_5N,_5O,_5P){if(_5O>=0){return new F(function(){return _5F(_5O,_5P);});}else{return _5N<=6?B(_5F(_5O,_5P)):[1,_5L,new T(function(){var _5Q=jsShowI(_5O),_5R=_5Q;return B(_5B(fromJSStr(_5R),[1,_5K,_5P]));})];}},_5S=[0,112],_5T=function(_5U){var _5V=new T(function(){return E(E(_5U)[2]);});return function(_5W,_){return [0,[1,_5S,new T(function(){return B(_5B(B(_5M(0,E(_5V)[1],_1g)),new T(function(){return E(E(_5U)[1]);},1)));})],new T(function(){var _5X=E(_5U);return [0,_5X[1],new T(function(){return [0,E(_5V)[1]+1|0];}),_5X[3],_5X[4],_5X[5],_5X[6],_5X[7]];})];};},_5Y=new T(function(){return B(unCStr("id"));}),_5Z=new T(function(){return B(unCStr("noid"));}),_60=[0,0],_61=false,_62=2,_63=[0],_64=new T(function(){return B(unCStr("Dynamic"));}),_65=new T(function(){return B(unCStr("Data.Dynamic"));}),_66=new T(function(){return B(unCStr("base"));}),_67=new T(function(){var _68=hs_wordToWord64(628307645),_69=_68,_6a=hs_wordToWord64(949574464),_6b=_6a;return [0,_69,_6b,[0,_69,_6b,_66,_65,_64],_1g];}),_6c=new T(function(){return B(unCStr("Haste.HPlay.View"));}),_6d=new T(function(){return B(unCStr("hplayground-0.1.2.9"));}),_6e=new T(function(){return B(unCStr("EventData"));}),_6f=new T(function(){var _6g=hs_wordToWord64(1145008931),_6h=_6g,_6i=hs_wordToWord64(2687009104),_6j=_6i;return [0,_6h,_6j,[0,_6h,_6j,_6d,_6c,_6e],_1g];}),_6k=[0],_6l=new T(function(){return B(unCStr("OnLoad"));}),_6m=[0,_6l,_6k],_6n=[0,_6f,_6m],_6o=[0,_67,_6n],_6p=function(_){return _5A;},_6q=function(_6r,_){return _5A;},_6s=[0,_6p,_6q],_6t=[0,_1g,_60,_62,_6s,_61,_6o,_63],_6u=function(_){var _=0,_6v=newMVar(),_6w=_6v,_=putMVar(_6w,_6t);return [0,_6w];},_6x=new T(function(){return B(_6(_6u));}),_6y=function(_6z,_6A,_){var _6B=jsFind(toJSStr(E(_6A))),_6C=_6B,_6D=E(_6C);if(!_6D[0]){var _6E=E(_6x)[1],_6F=takeMVar(_6E),_6G=_6F,_6H=B(A(_6z,[_6G,_])),_6I=_6H,_6J=E(_6I),_=putMVar(_6E,_6J[2]);return E(_6J[1])[2];}else{var _6K=E(_6D[1]),_6L=jsClearChildren(_6K[1]),_6M=E(_6x)[1],_6N=takeMVar(_6M),_6O=_6N,_6P=B(A(_6z,[_6O,_])),_6Q=_6P,_6R=E(_6Q),_6S=E(_6R[1]),_=putMVar(_6M,_6R[2]),_6T=B(A(_6S[1],[_6K,_])),_6U=_6T;return _6S[2];}},_6V=new T(function(){return B(unCStr("span"));}),_6W=function(_6X,_6Y,_6Z,_){var _70=jsCreateElem(toJSStr(E(_6V))),_71=_70,_72=jsAppendChild(_71,E(_6Z)[1]),_73=[0,_71],_74=B(A(_6X,[_6Y,_73,_])),_75=_74;return _73;},_76=function(_77,_78,_79,_){var _7a=B(A(_5T,[_79,_79,_])),_7b=_7a,_7c=E(_7b),_7d=_7c[1],_7e=E(_7c[2]),_7f=_7e[2],_7g=E(_7e[4]),_7h=B(A(_77,[[0,_7e[1],_7f,_7e[3],[0,function(_){return new F(function(){return _6y(function(_7i,_){var _7j=B(A(_77,[new T(function(){var _7k=E(_7i);return [0,_7k[1],_7f,_7k[3],_7k[4],_7k[5],_7k[6],_7k[7]];}),_])),_7l=_7j;return [0,[0,_K,E(E(_7l)[1])[2]],_7i];},_5Z,_);});},function(_7m,_){var _7n=B(_6y(new T(function(){return B(A(_78,[_7m]));},1),_7d,_)),_7o=_7n,_7p=E(_7o);return _7p[0]==0?_5A:B(A(_7g[2],[_7p[1],_]));}],_7e[5],_7e[6],_7e[7]],_])),_7q=_7h,_7r=E(_7q),_7s=_7r[2],_7t=E(_7r[1]),_7u=_7t[1],_7v=E(_7t[2]);if(!_7v[0]){return [0,[0,function(_7w,_){var _7x=B(A(_7u,[_7w,_])),_7y=_7x;if(!E(E(_79)[5])){var _7z=B(_6W(_Y,_K,_7w,_)),_7A=_7z,_7B=B(A(_d,[_t,_7A,_5Y,_7d,_])),_7C=_7B;return _7w;}else{return _7w;}},_5A],new T(function(){var _7D=E(_7s);return [0,_7D[1],_7D[2],_7D[3],_7g,_7D[5],_7D[6],_7D[7]];})];}else{var _7E=B(A(_78,[_7v[1],new T(function(){var _7F=E(_7s);return [0,_7F[1],_7F[2],_7F[3],_7g,_7F[5],_7F[6],_7F[7]];}),_])),_7G=_7E,_7H=E(_7G),_7I=E(_7H[1]),_7J=_7I[1];return [0,[0,function(_7K,_){var _7L=B(A(_7u,[_7K,_])),_7M=_7L;if(!E(E(_79)[5])){var _7N=B(_6W(_Y,_7J,_7K,_)),_7O=_7N,_7P=B(A(_d,[_t,_7O,_5Y,_7d,_])),_7Q=_7P;return _7K;}else{var _7R=B(A(_7J,[_7K,_])),_7S=_7R;return _7K;}},_7I[2]],_7H[2]];}},_7T=new T(function(){return B(unCStr("RouteShow"));}),_7U=new T(function(){return B(unCStr("RouteConfig"));}),_7V=function(_7W,_7X,_7Y){while(1){var _7Z=E(_7Y);if(!_7Z[0]){return [0,[0,_7W],_7X];}else{var _80=_7Z[2],_81=E(_7Z[1]),_82=_81[2],_83=E(_81[1])[1];if(_7W>=_83){if(_7W!=_83){_7Y=_80;continue;}else{_7W=_83;_7X=_82;_7Y=_80;continue;}}else{_7W=_83;_7X=_82;_7Y=_80;continue;}}}},_84=function(_85,_86,_87,_88){var _89=E(_87);if(!_89[0]){return E(_86);}else{var _8a=E(_88);if(!_8a[0]){return E(_86);}else{return new F(function(){return A(_85,[_89[1],_8a[1],new T(function(){return B(_84(_85,_86,_89[2],_8a[2]));})]);});}}},_8b=new T(function(){return B(unCStr("List.maximumBy: empty list"));}),_8c=new T(function(){return B(err(_8b));}),_8d=function(_8e,_8f){var _8g=E(_8f);return _8g[0]==0?[0]:[1,new T(function(){var _8h=B(_84(function(_8i,_8j,_8k){return [1,[0,new T(function(){return B(A(_8e,[_8i]));}),_8j],_8k];},_1g,_8g,_8g));if(!_8h[0]){var _8l=E(_8c);}else{var _8m=E(_8h[1]),_8n=_8m[1],_8o=_8m[2],_8p=E(_8h[2]);if(!_8p[0]){var _8q=[0,_8n,_8o];}else{var _8r=_8p[2],_8s=E(_8n)[1],_8t=E(_8p[1]),_8u=_8t[2],_8v=E(_8t[1])[1];if(_8s>=_8v){if(_8s!=_8v){var _8w=B(_7V(_8s,_8o,_8r)),_8x=[0,_8w[1],_8w[2]];}else{var _8y=B(_7V(_8v,_8u,_8r)),_8x=[0,_8y[1],_8y[2]];}var _8z=_8x,_8A=_8z;}else{var _8B=B(_7V(_8v,_8u,_8r)),_8A=[0,_8B[1],_8B[2]];}var _8C=_8A,_8D=_8C,_8E=_8D,_8F=_8E,_8q=_8F;}var _8G=_8q,_8l=_8G;}return _8l;})];},_8H=function(_8I){while(1){var _8J=(function(_8K){var _8L=E(_8K);if(!_8L[0]){return [0];}else{var _8M=_8L[2],_8N=E(_8L[1]);if(!_8N[0]){_8I=_8M;return null;}else{return [1,_8N[1],new T(function(){return B(_8H(_8M));})];}}})(_8I);if(_8J!=null){return _8J;}}},_8O=function(_8P,_8Q,_8R){while(1){var _8S=E(_8R);if(!_8S[0]){return [0,[0,_8P],_8Q];}else{var _8T=_8S[2],_8U=E(_8S[1]),_8V=_8U[2],_8W=E(_8U[1])[1];if(_8P>=_8W){if(_8P!=_8W){_8R=_8T;continue;}else{_8P=_8W;_8Q=_8V;_8R=_8T;continue;}}else{_8P=_8W;_8Q=_8V;_8R=_8T;continue;}}}},_8X=function(_8Y,_8Z){var _90=E(_8Z);return _90[0]==0?[0]:[1,new T(function(){return B(A(_8Y,[_90[1]]));}),new T(function(){return B(_8X(_8Y,_90[2]));})];},_91=function(_92,_93){var _94=B(_8H(B(_8X(function(_95){return new F(function(){return _8d(_92,_95);});},_93))));return _94[0]==0?[0]:[1,new T(function(){var _96=E(_94[1]),_97=_96[1],_98=_96[2],_99=E(_94[2]);if(!_99[0]){var _9a=[0,_97,_98];}else{var _9b=_99[2],_9c=E(_97)[1],_9d=E(_99[1]),_9e=_9d[2],_9f=E(_9d[1])[1];if(_9c>=_9f){if(_9c!=_9f){var _9g=B(_8O(_9c,_98,_9b)),_9h=[0,_9g[1],_9g[2]];}else{var _9i=B(_8O(_9f,_9e,_9b)),_9h=[0,_9i[1],_9i[2]];}var _9j=_9h,_9k=_9j;}else{var _9l=B(_8O(_9f,_9e,_9b)),_9k=[0,_9l[1],_9l[2]];}var _9m=_9k,_9n=_9m,_9o=_9n,_9p=_9o,_9a=_9p;}var _9q=_9a;return _9q;})];},_9r=function(_){return new F(function(){return A(_a,["(function(){return md51(jsRand().toString());})",_]);});},_9s=function(_){return new F(function(){return _9r(_);});},_9t=[0,0],_9u=function(_9v,_){var _9w=B(_9s(_)),_9x=_9w;return [0,[0,_K,[1,[0,_61,_9t,_1g,_5A,_9x]]],_9v];},_9y=[1,_c],_9z=[0,_K,_9y],_9A=function(_){var _=0,_9B=nMV(_1g),_9C=_9B;return [0,_9C];},_9D=new T(function(){return B(_6(_9A));}),_9E=function(_9F,_){var _=wMV(E(_9D)[1],_1g);return [0,_9z,_9F];},_9G=new T(function(){return B(unCStr("RouteCalculate"));}),_9H=new T(function(){return B(unAppCStr("invalid route in config state ",_9G));}),_9I=function(_9J,_){return new F(function(){return _0(_9H,_9J,_);});},_9K=new T(function(){return B(_5t(_9I));}),_9L=[0,_9K,_5A],_9M=function(_9N,_){return [0,_9L,_9N];},_9O=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_9P=new T(function(){return B(err(_9O));}),_9Q=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_9R=new T(function(){return B(err(_9Q));}),_9S=function(_9T,_9U){while(1){var _9V=E(_9T);if(!_9V[0]){return E(_9R);}else{var _9W=E(_9U);if(!_9W){return E(_9V[1]);}else{_9T=_9V[2];_9U=_9W-1|0;continue;}}}},_9X=function(_9Y,_9Z){while(1){var _a0=E(_9Y);if(!_a0[0]){return E(_9Z);}else{_9Y=_a0[2];var _a1=_9Z+E(_a0[1])[1]|0;_9Z=_a1;continue;}}},_a2=function(_a3){return E(E(_a3)[1])==(-1)?false:true;},_a4=function(_a5){return [0];},_a6=function(_a7,_a8){while(1){var _a9=(function(_aa,_ab){var _ac=E(_ab);if(!_ac[0]){return [0];}else{var _ad=_ac[1],_ae=_ac[2];if(!B(A(_aa,[_ad]))){var _af=_aa;_a8=_ae;_a7=_af;return null;}else{return [1,_ad,new T(function(){return B(_a6(_aa,_ae));})];}}})(_a7,_a8);if(_a9!=null){return _a9;}}},_ag=new T(function(){return B(unCStr(": empty list"));}),_ah=new T(function(){return B(unCStr("Prelude."));}),_ai=function(_aj){return new F(function(){return err(B(_5B(_ah,new T(function(){return B(_5B(_aj,_ag));},1))));});},_ak=new T(function(){return B(unCStr("tail"));}),_al=new T(function(){return B(_ai(_ak));}),_am=function(_an,_ao){var _ap=function(_aq){var _ar=E(_aq);if(!_ar[0]){return E(_a4);}else{var _as=new T(function(){return B(_ap(_ar[2]));}),_at=E(E(_ar[1])[1]);return _at==(-1)?E(_as):function(_au){var _av=E(_au);return _av[0]==0?[0]:[1,new T(function(){var _aw=E(_av[1])[1];return _aw>=0?!E(new T(function(){return _at<0;}))?B(_9S(new T(function(){return B(_9S(new T(function(){return E(E(_an)[1]);}),_at));}),_aw)):E(_9P):E(_9P);}),new T(function(){return B(A(_as,[_av[2]]));})];};}};return new F(function(){return _9X(B(A(_ap,[_ao,new T(function(){var _ax=B(_a6(_a2,_ao));return _ax[0]==0?E(_al):E(_ax[2]);})])),0);});},_ay=function(_az,_aA){return [0,B(_am(_az,_aA))];},_aB=function(_aC,_aD,_){return [0,[0,_K,[1,[1,_aC]]],_aD];},_aE=function(_aF,_aG,_){return [0,[0,_K,[1,[0,_aF]]],_aG];},_aH=function(_aI,_aJ,_aK,_){var _aL=B(_76(_aI,_aE,_aK,_)),_aM=_aL,_aN=E(_aM),_aO=E(_aN[1]),_aP=B(_76(_aJ,_aB,_aN[2],_)),_aQ=_aP,_aR=E(_aQ),_aS=E(_aR[1]);return [0,[0,function(_aT,_){var _aU=B(A(_aO[1],[_aT,_])),_aV=_aU,_aW=B(A(_aS[1],[_aT,_])),_aX=_aW;return _aT;},new T(function(){var _aY=E(_aO[2]);return _aY[0]==0?E(_aS[2]):E(_aY);})],_aR[2]];},_aZ=function(_b0){return new F(function(){return fromJSStr(E(_b0)[1]);});},_b1=function(_b2){return function(_b3,_b4){return new F(function(){return _0(new T(function(){return B(_aZ(_b2));}),_b3,_b4);});};},_b5=new T(function(){return B(unCStr("class"));}),_b6=new T(function(){return B(unCStr("col-md-1"));}),_b7=new T(function(){return B(unCStr("col-md-11"));}),_b8=new T(function(){return B(unCStr("row-fluid"));}),_b9=new T(function(){return [0,"\u041d\u0430\u0441\u0442\u0440\u043e\u0439\u043a\u0438 \u044d\u0432\u043e\u043b\u044e\u0446\u0438\u0438:"];}),_ba=new T(function(){return B(unCStr("style"));}),_bb=new T(function(){return B(unCStr("margin-top: 40px; font-size: 20px"));}),_bc=new T(function(){return [0,"\u0428\u0430\u043d\u0441 \u043c\u0443\u0442\u0430\u0446\u0438\u0438: "];}),_bd=new T(function(){return B(unCStr("label"));}),_be=function(_bf,_bg,_bh,_){var _bi=jsCreateElem(toJSStr(E(_bd))),_bj=_bi,_bk=jsAppendChild(_bj,E(_bh)[1]),_bl=[0,_bj],_bm=B(A(_bf,[_bg,_bl,_])),_bn=_bm;return _bl;},_bo=function(_bp,_){return new F(function(){return _be(_b1,_bc,_bp,_);});},_bq=new T(function(){return B(unCStr("row"));}),_br=new T(function(){return [0,"\u0432\u0435\u0440\u043e\u044f\u0442\u043d\u043e\u0441\u0442\u044c \u043d\u0435\u043a\u043e\u0440\u0440\u0435\u043a\u0442\u043d\u0430 [0, 1]"];}),_bs=[0,98],_bt=[1,_bs,_1g],_bu=function(_bv,_bw,_bx,_){var _by=jsCreateElem(toJSStr(_bt)),_bz=_by,_bA=jsAppendChild(_bz,E(_bx)[1]),_bB=[0,_bz],_bC=B(A(_bv,[_bw,_bB,_])),_bD=_bC;return _bB;},_bE=function(_bp,_){return new F(function(){return _bu(_b1,_br,_bp,_);});},_bF=[1,_bE],_bG=function(_bH,_bI,_){return [0,new T(function(){var _bJ=E(_bH)[1];if(_bJ<0){var _bK=E(_bF);}else{var _bK=_bJ>1?E(_bF):[0];}var _bL=_bK,_bM=_bL;return _bM;}),_bI];},_bN=new T(function(){return [0,"\u0427\u0430\u0441\u0442\u044c \u044d\u043b\u0438\u0442\u044b: "];}),_bO=function(_bp,_){return new F(function(){return _be(_b1,_bN,_bp,_);});},_bP=new T(function(){return [0,"\u0434\u043e\u043b\u044f \u043d\u0435\u043a\u043e\u0440\u0440\u0435\u043a\u0442\u043d\u0430 [0, 1]"];}),_bQ=function(_bp,_){return new F(function(){return _bu(_b1,_bP,_bp,_);});},_bR=[1,_bQ],_bS=function(_bT,_bU,_){return [0,new T(function(){var _bV=E(_bT)[1];if(_bV<0){var _bW=E(_bR);}else{var _bW=_bV>1?E(_bR):[0];}var _bX=_bW,_bY=_bX;return _bY;}),_bU];},_bZ=new T(function(){return [0,"\u041c\u0430\u043a\u0441 \u043f\u043e\u043a\u043e\u043b\u0435\u043d\u0438\u0439: "];}),_c0=function(_bp,_){return new F(function(){return _be(_b1,_bZ,_bp,_);});},_c1=new T(function(){return B(unCStr("div"));}),_c2=function(_c3,_c4,_c5,_){var _c6=jsCreateElem(toJSStr(E(_c1))),_c7=_c6,_c8=jsAppendChild(_c7,E(_c5)[1]),_c9=[0,_c7],_ca=B(A(_c3,[_c4,_c9,_])),_cb=_ca;return _c9;},_cc=function(_cd,_ce,_){var _cf=B(_c2(_Y,_cd,_ce,_)),_cg=_cf,_ch=B(A(_d,[_t,_cg,_b5,_bq,_])),_ci=_ch;return _cg;},_cj=new T(function(){return [0,"\u0434\u043e\u043b\u0436\u043d\u043e \u0431\u044b\u0442\u044c \u043f\u043e\u043b\u043e\u0436\u0438\u0442\u0435\u043b\u044c\u043d\u043e"];}),_ck=function(_bp,_){return new F(function(){return _bu(_b1,_cj,_bp,_);});},_cl=[1,_ck],_cm=function(_cn,_co,_){return [0,new T(function(){return E(_cn)[1]<=0?E(_cl):[0];}),_co];},_cp=new T(function(){return [0,"\u0427\u0438\u0441\u043b\u043e \u043f\u043e\u043f\u0443\u043b\u044f\u0446\u0438\u0439: "];}),_cq=function(_bp,_){return new F(function(){return _be(_b1,_cp,_bp,_);});},_cr=new T(function(){return [0,"\u0427\u0438\u0441\u043b\u043e \u0438\u043d\u0434\u0438\u0432\u0438\u0434\u043e\u0432 \u0432 \u043f\u043e\u043f\u0443\u043b\u044f\u0446\u0438\u0438: "];}),_cs=function(_bp,_){return new F(function(){return _be(_b1,_cr,_bp,_);});},_ct=function(_cu,_cv,_){return [0,[0,_K,[1,[1,_cu]]],_cv];},_cw=new T(function(){return [0,"\u041e\u0436\u0438\u0434\u0430\u0435\u043c\u044b\u0439 \u0444\u0438\u0442\u043d\u0435\u0441: "];}),_cx=function(_bp,_){return new F(function(){return _be(_b1,_cw,_bp,_);});},_cy=new T(function(){return B(unCStr("col-md-6"));}),_cz=[8,_],_cA=[13,_],_cB=new T(function(){return B(unCStr("wheel"));}),_cC=new T(function(){return B(unCStr("mouseout"));}),_cD=new T(function(){return B(unCStr("mouseover"));}),_cE=new T(function(){return B(unCStr("mousemove"));}),_cF=new T(function(){return B(unCStr("blur"));}),_cG=new T(function(){return B(unCStr("focus"));}),_cH=new T(function(){return B(unCStr("change"));}),_cI=new T(function(){return B(unCStr("unload"));}),_cJ=new T(function(){return B(unCStr("load"));}),_cK=new T(function(){return B(unCStr("submit"));}),_cL=new T(function(){return B(unCStr("keydown"));}),_cM=new T(function(){return B(unCStr("keyup"));}),_cN=new T(function(){return B(unCStr("keypress"));}),_cO=new T(function(){return B(unCStr("mouseup"));}),_cP=new T(function(){return B(unCStr("mousedown"));}),_cQ=new T(function(){return B(unCStr("dblclick"));}),_cR=new T(function(){return B(unCStr("click"));}),_cS=function(_cT){switch(E(_cT)[0]){case 0:return E(_cJ);case 1:return E(_cI);case 2:return E(_cH);case 3:return E(_cG);case 4:return E(_cF);case 5:return E(_cE);case 6:return E(_cD);case 7:return E(_cC);case 8:return E(_cR);case 9:return E(_cQ);case 10:return E(_cP);case 11:return E(_cO);case 12:return E(_cN);case 13:return E(_cM);case 14:return E(_cL);case 15:return E(_cK);default:return E(_cB);}},_cU=new T(function(){return B(unCStr("Control.Exception.Base"));}),_cV=new T(function(){return B(unCStr("base"));}),_cW=new T(function(){return B(unCStr("PatternMatchFail"));}),_cX=new T(function(){var _cY=hs_wordToWord64(18445595),_cZ=_cY,_d0=hs_wordToWord64(52003073),_d1=_d0;return [0,_cZ,_d1,[0,_cZ,_d1,_cV,_cU,_cW],_1g];}),_d2=function(_d3){return E(_cX);},_d4=function(_d5){return E(E(_d5)[1]);},_d6=function(_d7,_d8,_d9){var _da=B(A(_d7,[_])),_db=B(A(_d8,[_])),_dc=hs_eqWord64(_da[1],_db[1]),_dd=_dc;if(!E(_dd)){return [0];}else{var _de=hs_eqWord64(_da[2],_db[2]),_df=_de;return E(_df)==0?[0]:[1,_d9];}},_dg=function(_dh){var _di=E(_dh);return new F(function(){return _d6(B(_d4(_di[1])),_d2,_di[2]);});},_dj=function(_dk){return E(E(_dk)[1]);},_dl=function(_dm,_dn){return new F(function(){return _5B(E(_dm)[1],_dn);});},_do=[0,44],_dp=[0,93],_dq=[0,91],_dr=function(_ds,_dt,_du){var _dv=E(_dt);return _dv[0]==0?B(unAppCStr("[]",_du)):[1,_dq,new T(function(){return B(A(_ds,[_dv[1],new T(function(){var _dw=function(_dx){var _dy=E(_dx);return _dy[0]==0?E([1,_dp,_du]):[1,_do,new T(function(){return B(A(_ds,[_dy[1],new T(function(){return B(_dw(_dy[2]));})]));})];};return B(_dw(_dv[2]));})]));})];},_dz=function(_dA,_dB){return new F(function(){return _dr(_dl,_dA,_dB);});},_dC=function(_dD,_dE,_dF){return new F(function(){return _5B(E(_dE)[1],_dF);});},_dG=[0,_dC,_dj,_dz],_dH=new T(function(){return [0,_d2,_dG,_dI,_dg];}),_dI=function(_dJ){return [0,_dH,_dJ];},_dK=new T(function(){return B(unCStr("Non-exhaustive patterns in"));}),_dL=function(_dM,_dN){return new F(function(){return die(new T(function(){return B(A(_dN,[_dM]));}));});},_dO=function(_dP,_dQ){var _dR=E(_dQ);if(!_dR[0]){return [0,_1g,_1g];}else{var _dS=_dR[1];if(!B(A(_dP,[_dS]))){return [0,_1g,_dR];}else{var _dT=new T(function(){var _dU=B(_dO(_dP,_dR[2]));return [0,_dU[1],_dU[2]];});return [0,[1,_dS,new T(function(){return E(E(_dT)[1]);})],new T(function(){return E(E(_dT)[2]);})];}}},_dV=[0,32],_dW=[0,10],_dX=[1,_dW,_1g],_dY=function(_dZ){return E(E(_dZ)[1])==124?false:true;},_e0=function(_e1,_e2){var _e3=B(_dO(_dY,B(unCStr(_e1)))),_e4=_e3[1],_e5=function(_e6,_e7){return new F(function(){return _5B(_e6,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_5B(_e2,new T(function(){return B(_5B(_e7,_dX));},1)));})));},1));});},_e8=E(_e3[2]);if(!_e8[0]){return new F(function(){return _e5(_e4,_1g);});}else{return E(E(_e8[1])[1])==124?B(_e5(_e4,[1,_dV,_e8[2]])):B(_e5(_e4,_1g));}},_e9=function(_ea){return new F(function(){return _dL([0,new T(function(){return B(_e0(_ea,_dK));})],_dI);});},_eb=new T(function(){return B(_e9("src/Haste/HPlay/View.hs:(1065,9)-(1099,63)|case"));}),_ec=[0,_cJ,_6k],_ed=[0,_6f,_ec],_ee=[0,_cI,_6k],_ef=[0,_6f,_ee],_eg=[0,_cH,_6k],_eh=[0,_6f,_eg],_ei=[0,_cG,_6k],_ej=[0,_6f,_ei],_ek=[0,_cF,_6k],_el=[0,_6f,_ek],_em=[3],_en=[0,_cC,_em],_eo=[0,_6f,_en],_ep=function(_eq,_er){switch(E(_eq)[0]){case 0:return function(_){var _es=E(_6x)[1],_et=takeMVar(_es),_eu=_et,_=putMVar(_es,new T(function(){var _ev=E(_eu);return [0,_ev[1],_ev[2],_ev[3],_ev[4],_ev[5],_ed,_ev[7]];}));return new F(function(){return A(_er,[_]);});};case 1:return function(_){var _ew=E(_6x)[1],_ex=takeMVar(_ew),_ey=_ex,_=putMVar(_ew,new T(function(){var _ez=E(_ey);return [0,_ez[1],_ez[2],_ez[3],_ez[4],_ez[5],_ef,_ez[7]];}));return new F(function(){return A(_er,[_]);});};case 2:return function(_){var _eA=E(_6x)[1],_eB=takeMVar(_eA),_eC=_eB,_=putMVar(_eA,new T(function(){var _eD=E(_eC);return [0,_eD[1],_eD[2],_eD[3],_eD[4],_eD[5],_eh,_eD[7]];}));return new F(function(){return A(_er,[_]);});};case 3:return function(_){var _eE=E(_6x)[1],_eF=takeMVar(_eE),_eG=_eF,_=putMVar(_eE,new T(function(){var _eH=E(_eG);return [0,_eH[1],_eH[2],_eH[3],_eH[4],_eH[5],_ej,_eH[7]];}));return new F(function(){return A(_er,[_]);});};case 4:return function(_){var _eI=E(_6x)[1],_eJ=takeMVar(_eI),_eK=_eJ,_=putMVar(_eI,new T(function(){var _eL=E(_eK);return [0,_eL[1],_eL[2],_eL[3],_eL[4],_eL[5],_el,_eL[7]];}));return new F(function(){return A(_er,[_]);});};case 5:return function(_eM){return function(_){var _eN=E(_6x)[1],_eO=takeMVar(_eN),_eP=_eO,_=putMVar(_eN,new T(function(){var _eQ=E(_eP);return [0,_eQ[1],_eQ[2],_eQ[3],_eQ[4],_eQ[5],[0,_6f,[0,_cE,[2,E(_eM)]]],_eQ[7]];}));return new F(function(){return A(_er,[_]);});};};case 6:return function(_eR){return function(_){var _eS=E(_6x)[1],_eT=takeMVar(_eS),_eU=_eT,_=putMVar(_eS,new T(function(){var _eV=E(_eU);return [0,_eV[1],_eV[2],_eV[3],_eV[4],_eV[5],[0,_6f,[0,_cD,[2,E(_eR)]]],_eV[7]];}));return new F(function(){return A(_er,[_]);});};};case 7:return function(_){var _eW=E(_6x)[1],_eX=takeMVar(_eW),_eY=_eX,_=putMVar(_eW,new T(function(){var _eZ=E(_eY);return [0,_eZ[1],_eZ[2],_eZ[3],_eZ[4],_eZ[5],_eo,_eZ[7]];}));return new F(function(){return A(_er,[_]);});};case 8:return function(_f0,_f1){return function(_){var _f2=E(_6x)[1],_f3=takeMVar(_f2),_f4=_f3,_=putMVar(_f2,new T(function(){var _f5=E(_f4);return [0,_f5[1],_f5[2],_f5[3],_f5[4],_f5[5],[0,_6f,[0,_cR,[1,_f0,E(_f1)]]],_f5[7]];}));return new F(function(){return A(_er,[_]);});};};case 9:return function(_f6,_f7){return function(_){var _f8=E(_6x)[1],_f9=takeMVar(_f8),_fa=_f9,_=putMVar(_f8,new T(function(){var _fb=E(_fa);return [0,_fb[1],_fb[2],_fb[3],_fb[4],_fb[5],[0,_6f,[0,_cQ,[1,_f6,E(_f7)]]],_fb[7]];}));return new F(function(){return A(_er,[_]);});};};case 10:return function(_fc,_fd){return function(_){var _fe=E(_6x)[1],_ff=takeMVar(_fe),_fg=_ff,_=putMVar(_fe,new T(function(){var _fh=E(_fg);return [0,_fh[1],_fh[2],_fh[3],_fh[4],_fh[5],[0,_6f,[0,_cP,[1,_fc,E(_fd)]]],_fh[7]];}));return new F(function(){return A(_er,[_]);});};};case 11:return function(_fi,_fj){return function(_){var _fk=E(_6x)[1],_fl=takeMVar(_fk),_fm=_fl,_=putMVar(_fk,new T(function(){var _fn=E(_fm);return [0,_fn[1],_fn[2],_fn[3],_fn[4],_fn[5],[0,_6f,[0,_cO,[1,_fi,E(_fj)]]],_fn[7]];}));return new F(function(){return A(_er,[_]);});};};case 12:return function(_fo,_){var _fp=E(_6x)[1],_fq=takeMVar(_fp),_fr=_fq,_=putMVar(_fp,new T(function(){var _fs=E(_fr);return [0,_fs[1],_fs[2],_fs[3],_fs[4],_fs[5],[0,_6f,[0,_cN,[4,_fo]]],_fs[7]];}));return new F(function(){return A(_er,[_]);});};case 13:return function(_ft,_){var _fu=E(_6x)[1],_fv=takeMVar(_fu),_fw=_fv,_=putMVar(_fu,new T(function(){var _fx=E(_fw);return [0,_fx[1],_fx[2],_fx[3],_fx[4],_fx[5],[0,_6f,[0,_cM,[4,_ft]]],_fx[7]];}));return new F(function(){return A(_er,[_]);});};case 14:return function(_fy,_){var _fz=E(_6x)[1],_fA=takeMVar(_fz),_fB=_fA,_=putMVar(_fz,new T(function(){var _fC=E(_fB);return [0,_fC[1],_fC[2],_fC[3],_fC[4],_fC[5],[0,_6f,[0,_cL,[4,_fy]]],_fC[7]];}));return new F(function(){return A(_er,[_]);});};default:return E(_eb);}},_fD=[0,_cS,_ep],_fE=new T(function(){return B(unCStr("size"));}),_fF=[0,50],_fG=[1,_fF,_1g],_fH=[0,_K,_5A],_fI=true,_fJ=[1,_c],_fK=[0,_K,_fJ],_fL=function(_fM){var _fN=new T(function(){return E(E(_fM)[2]);});return function(_b3,_b4){return new F(function(){return _76(function(_fO,_){return [0,_fK,new T(function(){var _fP=E(_fM);return [0,_fP[1],new T(function(){return [0,E(_fN)[1]+1|0];}),_fP[3],_fP[4],_fP[5],_fP[6],_fP[7]];})];},function(_fQ,_fR,_){return [0,[0,_K,[1,[1,_5S,new T(function(){return B(_5B(B(_5M(0,E(_fN)[1],_1g)),new T(function(){return E(E(_fM)[1]);},1)));})]]],_fR];},_b3,_b4);});};},_fS=function(_fT,_){return [0,[0,_K,[1,_fT]],_fT];},_fU=function(_5m,_){return new F(function(){return _76(_fS,_fL,_5m,_);});},_fV=new T(function(){return B(unCStr(" could be found!"));}),_fW=function(_fX){return new F(function(){return err(B(unAppCStr("No element with ID ",new T(function(){return B(_5B(_fX,_fV));}))));});},_fY=function(_fZ,_g0,_g1,_){var _g2=B(_76(_fU,function(_g3,_g4,_){return new F(function(){return _76(function(_g5,_){return [0,[0,function(_g6,_){var _g7=B(_6W(_Y,_K,_g6,_)),_g8=_g7,_g9=B(A(_d,[_t,_g8,_5Y,_g3,_])),_ga=_g9;return _g8;},_fJ],_g5];},function(_gb,_5m,_){return new F(function(){return (function(_5m,_){return new F(function(){return _76(_fZ,function(_gc){return function(_gd,_){var _ge=B(A(new T(function(){return B(A(_g0,[_gc]));}),[_gd,_])),_gf=_ge,_gg=E(_gf),_gh=_gg[2],_gi=E(_gg[1]);if(!_gi[0]){var _gj=E(_g3),_gk=jsFind(toJSStr(_gj)),_gl=_gk,_gm=E(_gl);if(!_gm[0]){return new F(function(){return _fW(_gj);});}else{var _gn=jsClearChildren(E(_gm[1])[1]);return [0,[0,_K,[1,_gc]],_gh];}}else{var _go=E(_g3),_gp=jsFind(toJSStr(_go)),_gq=_gp,_gr=E(_gq);if(!_gr[0]){return new F(function(){return _fW(_go);});}else{var _gs=E(_gr[1]),_gt=jsClearChildren(_gs[1]),_gu=B(A(_5t,[_gi[1],_gs,_])),_gv=_gu;return [0,_fH,_gh];}}};},_5m,_);});})(_5m,_);});},_g4,_);});},new T(function(){var _gw=E(_g1);return [0,_gw[1],_gw[2],_gw[3],_gw[4],_fI,_gw[6],_gw[7]];}),_)),_gx=_g2;return [0,new T(function(){return E(E(_gx)[1]);}),new T(function(){var _gy=E(E(_gx)[2]);return [0,_gy[1],_gy[2],_gy[3],_gy[4],new T(function(){return E(E(_g1)[5]);}),_gy[6],_gy[7]];})];},_gz=new T(function(){return B(unCStr("button"));}),_gA=function(_gB,_gC,_gD,_){var _gE=jsCreateElem(toJSStr(E(_gz))),_gF=_gE,_gG=jsAppendChild(_gF,E(_gD)[1]),_gH=[0,_gF],_gI=B(A(_gB,[_gC,_gH,_])),_gJ=_gI;return _gH;},_gK=new T(function(){return B(unCStr("margin-right: 10px; margin-left: 10px; margin-top: 3px"));}),_gL=new T(function(){return B(unCStr("button"));}),_gM=new T(function(){return B(unCStr("type"));}),_gN=new T(function(){return B(unCStr("btn btn-primary"));}),_gO=new T(function(){return B(unCStr("class"));}),_gP=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_gQ=new T(function(){return B(err(_gP));}),_gR=function(_gS,_gT,_gU,_){var _gV=B(A(_gS,[_gU,_])),_gW=_gV,_gX=E(_gW),_gY=E(_gX[1]);return [0,[0,function(_gZ,_){var _h0=B(A(_gY[1],[_gZ,_])),_h1=_h0,_h2=jsFind(toJSStr(E(_gT))),_h3=_h2;return new T(function(){var _h4=E(_h3);return _h4[0]==0?E(_gQ):E(_h4[1]);});},_gY[2]],_gX[2]];},_h5=function(_h6,_h7){while(1){var _h8=E(_h6);if(!_h8[0]){return E(_h7)[0]==0?true:false;}else{var _h9=E(_h7);if(!_h9[0]){return false;}else{if(E(_h8[1])[1]!=E(_h9[1])[1]){return false;}else{_h6=_h8[2];_h7=_h9[2];continue;}}}}},_ha=function(_hb){return E(_6f);},_hc=function(_hd,_he,_hf,_hg){var _hh=B(A(_hd,[_])),_hi=hs_eqWord64(_he,_hh[1]),_hj=_hi;if(!E(_hj)){return [0];}else{var _hk=hs_eqWord64(_hf,_hh[2]),_hl=_hk;return E(_hl)==0?[0]:[1,_hg];}},_hm=function(_hn,_){return [0,[0,_K,new T(function(){var _ho=E(E(_hn)[6]),_hp=E(_ho[1]);return B(_hc(_ha,_hp[1],_hp[2],_ho[2]));})],_hn];},_hq=new T(function(){return B(unCStr("Onload"));}),_hr=[0,_hq,_6k],_hs=[0,_6f,_hr],_ht=function(_hu,_hv,_){return [0,_fK,new T(function(){var _hw=E(_hu);return [0,_hw[1],_hw[2],_hw[3],_hw[4],_hw[5],_hs,_hw[7]];})];},_hx=function(_5m,_){return new F(function(){return _76(_fS,_ht,_5m,_);});},_hy=[0,_K,_5A],_hz=function(_hA,_hB,_){var _hC=B(A(_hA,[new T(function(){var _hD=E(_hB);return [0,_hD[1],_hD[2],_hD[3],_hD[4],_fI,_hD[6],_hD[7]];}),_])),_hE=_hC;return [0,new T(function(){return E(E(_hE)[1]);}),new T(function(){var _hF=E(E(_hE)[2]);return [0,_hF[1],_hF[2],_hF[3],_hF[4],new T(function(){return E(E(_hB)[5]);}),_hF[6],_hF[7]];})];},_hG=function(_hH){return E(E(_hH)[2]);},_hI=function(_hJ){return E(E(_hJ)[1]);},_hK=function(_hL,_hM,_hN){return function(_hO,_){var _hP=B(A(_hM,[_hO,_])),_hQ=_hP,_hR=E(_hQ),_hS=E(_hR[1]);return [0,[0,function(_hT,_){var _hU=B(A(_hS[1],[_hT,_])),_hV=_hU,_hW=E(_hV),_hX=jsSetCB(_hW[1],E(new T(function(){return [0,toJSStr(B(A(_hI,[_hL,_hN])))];}))[1],E(new T(function(){return B(A(new T(function(){return B(_hG(_hL));}),[_hN,function(_){var _hY=E(E(_hO)[4]),_hZ=B(A(_hY[1],[_])),_i0=_hZ,_i1=E(_i0);if(!_i1[0]){return _c;}else{var _i2=B(A(_hY[2],[_i1[1],_])),_i3=_i2;return _c;}}]));}))),_i4=_hX;return _hW;},_hS[2]],_hR[2]];};},_i5=function(_i6,_i7){return function(_b3,_b4){return new F(function(){return _hz(function(_5m,_){return new F(function(){return _76(_hx,function(_i8,_5m,_){return new F(function(){return (function(_5m,_){return new F(function(){return _76(new T(function(){return B(_hK(_fD,function(_i9,_){return [0,[0,_i6,_fJ],_i9];},_i7));}),function(_ia){return function(_5m,_){return new F(function(){return _76(_hm,function(_ib){var _ic=E(_ib);return new F(function(){return (function(_id,_ie){return function(_if,_){return !E(new T(function(){return B(_h5(new T(function(){return B(_cS(_i7));}),_id));}))?[0,_hy,_if]:[0,[0,_K,[1,[0,_id,_ie]]],_if];};})(_ic[1],_ic[2]);});},_5m,_);});};},_5m,_);});})(_5m,_);});},_5m,_);});},_b3,_b4);});};},_ig=function(_ih,_ii){return function(_b3,_b4){return new F(function(){return _hz(function(_ij,_){return new F(function(){return _gR(function(_ik,_){return new F(function(){return _76(new T(function(){return B(_i5(function(_il,_){var _im=B(_gA(_0,_ii,_il,_)),_in=_im,_io=B(A(_d,[_t,_in,_5Y,_ii,_])),_ip=_io,_iq=B(A(_d,[_t,_in,_gO,_gN,_])),_ir=_iq,_is=B(A(_d,[_t,_in,_gM,_gL,_])),_it=_is,_iu=B(A(_d,[_t,_in,_17,_gK,_])),_iv=_iu;return _in;},_cz));}),function(_iw,_ix,_){return [0,[0,_K,[1,_ih]],_ix];},_ik,_);});},_ii,_ij,_);});},_b3,_b4);});};},_iy=new T(function(){return B(unCStr("text"));}),_iz=[0,43],_iA=[1,_iz,_1g],_iB=[0,45],_iC=[1,_iB,_1g],_iD=function(_iE,_iF,_iG,_){var _iH=B(_5n(_iE,_iG,_)),_iI=_iH,_iJ=B(A(_iF,[_iI,_])),_iK=_iJ;return _iI;},_iL=new T(function(){return B(unCStr("()"));}),_iM=new T(function(){return B(unCStr("GHC.Tuple"));}),_iN=new T(function(){return B(unCStr("ghc-prim"));}),_iO=new T(function(){var _iP=hs_wordToWord64(2170319554),_iQ=_iP,_iR=hs_wordToWord64(26914641),_iS=_iR;return [0,_iQ,_iS,[0,_iQ,_iS,_iN,_iM,_iL],_1g];}),_iT=function(_iU){return E(_iO);},_iV=new T(function(){return B(unCStr("PerchM"));}),_iW=new T(function(){return B(unCStr("Haste.Perch"));}),_iX=new T(function(){return B(unCStr("haste-perch-0.1.0.8"));}),_iY=new T(function(){var _iZ=hs_wordToWord64(3848245492),_j0=_iZ,_j1=hs_wordToWord64(243906750),_j2=_j1;return [0,_j0,_j2,[0,_j0,_j2,_iX,_iW,_iV],_1g];}),_j3=function(_j4){return E(_iY);},_j5=function(_j6){var _j7=E(_j6);if(!_j7[0]){return [0];}else{var _j8=E(_j7[1]);return [1,[0,_j8[1],_j8[2]],new T(function(){return B(_j5(_j7[2]));})];}},_j9=function(_ja,_jb){var _jc=E(_ja);if(!_jc){return [0,_1g,_jb];}else{var _jd=E(_jb);if(!_jd[0]){return [0,_1g,_1g];}else{var _je=new T(function(){var _jf=B(_j9(_jc-1|0,_jd[2]));return [0,_jf[1],_jf[2]];});return [0,[1,_jd[1],new T(function(){return E(E(_je)[1]);})],new T(function(){return E(E(_je)[2]);})];}}},_jg=[0,120],_jh=[0,48],_ji=function(_jj){var _jk=new T(function(){var _jl=B(_j9(8,new T(function(){var _jm=md5(toJSStr(E(_jj))),_jn=_jm;return fromJSStr(_jn);})));return [0,_jl[1],_jl[2]];}),_jo=parseInt([0,toJSStr([1,_jh,[1,_jg,new T(function(){return E(E(_jk)[1]);})]])]),_jp=_jo,_jq=new T(function(){var _jr=B(_j9(8,new T(function(){return E(E(_jk)[2]);})));return [0,_jr[1],_jr[2]];}),_js=parseInt([0,toJSStr([1,_jh,[1,_jg,new T(function(){return E(E(_jq)[1]);})]])]),_jt=_js,_ju=hs_mkWord64(_jp,_jt),_jv=_ju,_jw=parseInt([0,toJSStr([1,_jh,[1,_jg,new T(function(){return E(B(_j9(8,new T(function(){return E(E(_jq)[2]);})))[1]);})]])]),_jx=_jw,_jy=hs_mkWord64(_jx,_jx),_jz=_jy;return [0,_jv,_jz];},_jA=function(_jB){var _jC=E(_jB);if(!_jC[0]){return [0];}else{return new F(function(){return _5B(_jC[1],new T(function(){return B(_jA(_jC[2]));},1));});}},_jD=function(_jE,_jF){var _jG=jsShowI(_jE),_jH=_jG,_jI=md5(_jH),_jJ=_jI;return new F(function(){return _5B(fromJSStr(_jJ),new T(function(){var _jK=jsShowI(_jF),_jL=_jK,_jM=md5(_jL),_jN=_jM;return fromJSStr(_jN);},1));});},_jO=function(_jP){var _jQ=E(_jP);return new F(function(){return _jD(_jQ[1],_jQ[2]);});},_jR=function(_jS,_jT){return function(_jU){return E(new T(function(){var _jV=B(A(_jS,[_])),_jW=E(_jV[3]),_jX=_jW[1],_jY=_jW[2],_jZ=B(_5B(_jV[4],[1,new T(function(){return B(A(_jT,[_]));}),_1g]));if(!_jZ[0]){var _k0=[0,_jX,_jY,_jW,_1g];}else{var _k1=B(_ji(new T(function(){return B(_jA(B(_8X(_jO,[1,[0,_jX,_jY],new T(function(){return B(_j5(_jZ));})]))));},1))),_k0=[0,_k1[1],_k1[2],_jW,_jZ];}var _k2=_k0,_k3=_k2;return _k3;}));};},_k4=new T(function(){return B(_jR(_j3,_iT));}),_k5=new T(function(){return B(unCStr("value"));}),_k6=new T(function(){return B(unCStr("id"));}),_k7=new T(function(){return B(unCStr("onclick"));}),_k8=new T(function(){return B(unCStr("checked"));}),_k9=[0,_k8,_1g],_ka=[1,_k9,_1g],_kb=new T(function(){return B(unCStr("type"));}),_kc=new T(function(){return B(unCStr("input"));}),_kd=function(_ke,_){return new F(function(){return _5n(_kc,_ke,_);});},_kf=function(_kg,_kh,_ki,_kj,_kk){var _kl=new T(function(){var _km=new T(function(){return B(_5f(_kd,[1,[0,_kb,_kh],[1,[0,_k6,_kg],[1,[0,_k5,_ki],_1g]]]));});return !E(_kj)?E(_km):B(_5f(_km,_ka));}),_kn=E(_kk);return _kn[0]==0?E(_kl):B(_5f(_kl,[1,[0,_k7,_kn[1]],_1g]));},_ko=new T(function(){return B(unCStr("href"));}),_kp=[0,97],_kq=[1,_kp,_1g],_kr=function(_ks,_){return new F(function(){return _5n(_kq,_ks,_);});},_kt=function(_ku,_kv){return function(_kw,_){var _kx=B(A(new T(function(){return B(_5f(_kr,[1,[0,_ko,_ku],_1g]));}),[_kw,_])),_ky=_kx,_kz=B(A(_kv,[_ky,_])),_kA=_kz;return _ky;};},_kB=function(_kC){return new F(function(){return _kt(_kC,function(_5m,_){return new F(function(){return _0(_kC,_5m,_);});});});},_kD=new T(function(){return B(unCStr("option"));}),_kE=function(_kF,_){return new F(function(){return _5n(_kD,_kF,_);});},_kG=new T(function(){return B(unCStr("selected"));}),_kH=[0,_kG,_1g],_kI=[1,_kH,_1g],_kJ=function(_kK,_kL,_kM){var _kN=new T(function(){return B(_5f(_kE,[1,[0,_k5,_kK],_1g]));});if(!E(_kM)){return function(_kO,_){var _kP=B(A(_kN,[_kO,_])),_kQ=_kP,_kR=B(A(_kL,[_kQ,_])),_kS=_kR;return _kQ;};}else{return new F(function(){return _5f(function(_kT,_){var _kU=B(A(_kN,[_kT,_])),_kV=_kU,_kW=B(A(_kL,[_kV,_])),_kX=_kW;return _kV;},_kI);});}},_kY=function(_kZ,_l0){return new F(function(){return _kJ(_kZ,function(_5m,_){return new F(function(){return _0(_kZ,_5m,_);});},_l0);});},_l1=new T(function(){return B(unCStr("method"));}),_l2=new T(function(){return B(unCStr("action"));}),_l3=new T(function(){return B(unCStr("UTF-8"));}),_l4=new T(function(){return B(unCStr("acceptCharset"));}),_l5=[0,_l4,_l3],_l6=new T(function(){return B(unCStr("form"));}),_l7=function(_l8,_){return new F(function(){return _5n(_l6,_l8,_);});},_l9=function(_la,_lb,_lc){return function(_ld,_){var _le=B(A(new T(function(){return B(_5f(_l7,[1,_l5,[1,[0,_l2,_la],[1,[0,_l1,_lb],_1g]]]));}),[_ld,_])),_lf=_le,_lg=B(A(_lc,[_lf,_])),_lh=_lg;return _lf;};},_li=new T(function(){return B(unCStr("select"));}),_lj=function(_lk,_){return new F(function(){return _5n(_li,_lk,_);});},_ll=function(_lm,_ln){return function(_lo,_){var _lp=B(A(new T(function(){return B(_5f(_lj,[1,[0,_k6,_lm],_1g]));}),[_lo,_])),_lq=_lp,_lr=B(A(_ln,[_lq,_])),_ls=_lr;return _lq;};},_lt=new T(function(){return B(unCStr("textarea"));}),_lu=function(_lv,_){return new F(function(){return _5n(_lt,_lv,_);});},_lw=function(_lx,_ly){return function(_lz,_){var _lA=B(A(new T(function(){return B(_5f(_lu,[1,[0,_k6,_lx],_1g]));}),[_lz,_])),_lB=_lA,_lC=B(_0(_ly,_lB,_)),_lD=_lC;return _lB;};},_lE=function(_lF,_lG,_){var _lH=E(_lF);if(!_lH[0]){return _lG;}else{var _lI=B(A(_lH[1],[_lG,_])),_lJ=_lI,_lK=B(_lE(_lH[2],_lG,_)),_lL=_lK;return _lG;}},_lM=function(_lN,_lO,_){return new F(function(){return _lE(_lN,_lO,_);});},_lP=function(_lQ,_lR,_lS,_){var _lT=B(A(_lQ,[_lS,_])),_lU=_lT,_lV=B(A(_lR,[_lS,_])),_lW=_lV;return _lS;},_lX=[0,_K,_lP,_lM],_lY=[0,_lX,_k4,_0,_0,_iD,_5t,_kt,_kB,_kf,_lw,_ll,_kJ,_kY,_l9,_5f],_lZ=new T(function(){return B(unCStr("GHC.IO.Exception"));}),_m0=new T(function(){return B(unCStr("base"));}),_m1=new T(function(){return B(unCStr("IOException"));}),_m2=new T(function(){var _m3=hs_wordToWord64(4053623282),_m4=_m3,_m5=hs_wordToWord64(3693590983),_m6=_m5;return [0,_m4,_m6,[0,_m4,_m6,_m0,_lZ,_m1],_1g];}),_m7=function(_m8){return E(_m2);},_m9=function(_ma){var _mb=E(_ma);return new F(function(){return _d6(B(_d4(_mb[1])),_m7,_mb[2]);});},_mc=new T(function(){return B(unCStr(": "));}),_md=[0,41],_me=new T(function(){return B(unCStr(" ("));}),_mf=new T(function(){return B(unCStr("already exists"));}),_mg=new T(function(){return B(unCStr("does not exist"));}),_mh=new T(function(){return B(unCStr("protocol error"));}),_mi=new T(function(){return B(unCStr("failed"));}),_mj=new T(function(){return B(unCStr("invalid argument"));}),_mk=new T(function(){return B(unCStr("inappropriate type"));}),_ml=new T(function(){return B(unCStr("hardware fault"));}),_mm=new T(function(){return B(unCStr("unsupported operation"));}),_mn=new T(function(){return B(unCStr("timeout"));}),_mo=new T(function(){return B(unCStr("resource vanished"));}),_mp=new T(function(){return B(unCStr("interrupted"));}),_mq=new T(function(){return B(unCStr("resource busy"));}),_mr=new T(function(){return B(unCStr("resource exhausted"));}),_ms=new T(function(){return B(unCStr("end of file"));}),_mt=new T(function(){return B(unCStr("illegal operation"));}),_mu=new T(function(){return B(unCStr("permission denied"));}),_mv=new T(function(){return B(unCStr("user error"));}),_mw=new T(function(){return B(unCStr("unsatisified constraints"));}),_mx=new T(function(){return B(unCStr("system error"));}),_my=function(_mz,_mA){switch(E(_mz)){case 0:return new F(function(){return _5B(_mf,_mA);});break;case 1:return new F(function(){return _5B(_mg,_mA);});break;case 2:return new F(function(){return _5B(_mq,_mA);});break;case 3:return new F(function(){return _5B(_mr,_mA);});break;case 4:return new F(function(){return _5B(_ms,_mA);});break;case 5:return new F(function(){return _5B(_mt,_mA);});break;case 6:return new F(function(){return _5B(_mu,_mA);});break;case 7:return new F(function(){return _5B(_mv,_mA);});break;case 8:return new F(function(){return _5B(_mw,_mA);});break;case 9:return new F(function(){return _5B(_mx,_mA);});break;case 10:return new F(function(){return _5B(_mh,_mA);});break;case 11:return new F(function(){return _5B(_mi,_mA);});break;case 12:return new F(function(){return _5B(_mj,_mA);});break;case 13:return new F(function(){return _5B(_mk,_mA);});break;case 14:return new F(function(){return _5B(_ml,_mA);});break;case 15:return new F(function(){return _5B(_mm,_mA);});break;case 16:return new F(function(){return _5B(_mn,_mA);});break;case 17:return new F(function(){return _5B(_mo,_mA);});break;default:return new F(function(){return _5B(_mp,_mA);});}},_mB=[0,125],_mC=new T(function(){return B(unCStr("{handle: "));}),_mD=function(_mE,_mF,_mG,_mH,_mI,_mJ){var _mK=new T(function(){var _mL=new T(function(){return B(_my(_mF,new T(function(){var _mM=E(_mH);return _mM[0]==0?E(_mJ):B(_5B(_me,new T(function(){return B(_5B(_mM,[1,_md,_mJ]));},1)));},1)));},1),_mN=E(_mG);return _mN[0]==0?E(_mL):B(_5B(_mN,new T(function(){return B(_5B(_mc,_mL));},1)));},1),_mO=E(_mI);if(!_mO[0]){var _mP=E(_mE);if(!_mP[0]){return E(_mK);}else{var _mQ=E(_mP[1]);return _mQ[0]==0?B(_5B(_mC,new T(function(){return B(_5B(_mQ[1],[1,_mB,new T(function(){return B(_5B(_mc,_mK));})]));},1))):B(_5B(_mC,new T(function(){return B(_5B(_mQ[1],[1,_mB,new T(function(){return B(_5B(_mc,_mK));})]));},1)));}}else{return new F(function(){return _5B(_mO[1],new T(function(){return B(_5B(_mc,_mK));},1));});}},_mR=function(_mS){var _mT=E(_mS);return new F(function(){return _mD(_mT[1],_mT[2],_mT[3],_mT[4],_mT[6],_1g);});},_mU=function(_mV,_mW){var _mX=E(_mV);return new F(function(){return _mD(_mX[1],_mX[2],_mX[3],_mX[4],_mX[6],_mW);});},_mY=function(_mZ,_n0){return new F(function(){return _dr(_mU,_mZ,_n0);});},_n1=function(_n2,_n3,_n4){var _n5=E(_n3);return new F(function(){return _mD(_n5[1],_n5[2],_n5[3],_n5[4],_n5[6],_n4);});},_n6=[0,_n1,_mR,_mY],_n7=new T(function(){return [0,_m7,_n6,_n8,_m9];}),_n8=function(_n9){return [0,_n7,_n9];},_na=7,_nb=function(_nc){return [0,_5A,_na,_1g,_nc,_5A,_5A];},_nd=function(_ne,_){return new F(function(){return die(new T(function(){return B(_n8(new T(function(){return B(_nb(_ne));})));}));});},_nf=function(_ng,_){return new F(function(){return _nd(_ng,_);});},_nh=function(_ni,_){return new F(function(){return _nf(_ni,_);});},_nj=function(_nk,_){return new F(function(){return _nh(_nk,_);});},_nl=function(_nm,_nn,_){var _no=B(A(_nm,[_])),_np=_no;return new F(function(){return A(_nn,[_np,_]);});},_nq=function(_nr,_ns,_){var _nt=B(A(_nr,[_])),_nu=_nt;return new F(function(){return A(_ns,[_]);});},_nv=[0,_nl,_nq,_K,_nj],_nw=[0,_nv,_t],_nx=new T(function(){return B(_e9("Text/ParserCombinators/ReadP.hs:(134,3)-(157,60)|function mplus"));}),_ny=function(_nz,_nA){while(1){var _nB=(function(_nC,_nD){var _nE=E(_nC);switch(_nE[0]){case 0:var _nF=E(_nD);if(!_nF[0]){return [0];}else{_nz=B(A(_nE[1],[_nF[1]]));_nA=_nF[2];return null;}break;case 1:var _nG=B(A(_nE[1],[_nD])),_nH=_nD;_nz=_nG;_nA=_nH;return null;case 2:return [0];case 3:return [1,[0,_nE[1],_nD],new T(function(){return B(_ny(_nE[2],_nD));})];default:return E(_nE[1]);}})(_nz,_nA);if(_nB!=null){return _nB;}}},_nI=function(_nJ,_nK){var _nL=function(_nM){var _nN=E(_nK);if(_nN[0]==3){return [3,_nN[1],new T(function(){return B(_nI(_nJ,_nN[2]));})];}else{var _nO=E(_nJ);if(_nO[0]==2){return E(_nN);}else{var _nP=E(_nN);if(_nP[0]==2){return E(_nO);}else{var _nQ=function(_nR){var _nS=E(_nP);if(_nS[0]==4){return [1,function(_nT){return [4,new T(function(){return B(_5B(B(_ny(_nO,_nT)),_nS[1]));})];}];}else{var _nU=E(_nO);if(_nU[0]==1){var _nV=_nU[1],_nW=E(_nS);return _nW[0]==0?[1,function(_nX){return new F(function(){return _nI(B(A(_nV,[_nX])),_nW);});}]:[1,function(_nY){return new F(function(){return _nI(B(A(_nV,[_nY])),new T(function(){return B(A(_nW[1],[_nY]));}));});}];}else{var _nZ=E(_nS);return _nZ[0]==0?E(_nx):[1,function(_o0){return new F(function(){return _nI(_nU,new T(function(){return B(A(_nZ[1],[_o0]));}));});}];}}},_o1=E(_nO);switch(_o1[0]){case 1:var _o2=E(_nP);if(_o2[0]==4){return [1,function(_o3){return [4,new T(function(){return B(_5B(B(_ny(B(A(_o1[1],[_o3])),_o3)),_o2[1]));})];}];}else{return new F(function(){return _nQ(_);});}break;case 4:var _o4=_o1[1],_o5=E(_nP);switch(_o5[0]){case 0:return [1,function(_o6){return [4,new T(function(){return B(_5B(_o4,new T(function(){return B(_ny(_o5,_o6));},1)));})];}];case 1:return [1,function(_o7){return [4,new T(function(){return B(_5B(_o4,new T(function(){return B(_ny(B(A(_o5[1],[_o7])),_o7));},1)));})];}];default:return [4,new T(function(){return B(_5B(_o4,_o5[1]));})];}break;default:return new F(function(){return _nQ(_);});}}}}},_o8=E(_nJ);switch(_o8[0]){case 0:var _o9=E(_nK);if(!_o9[0]){return [0,function(_oa){return new F(function(){return _nI(B(A(_o8[1],[_oa])),new T(function(){return B(A(_o9[1],[_oa]));}));});}];}else{return new F(function(){return _nL(_);});}break;case 3:return [3,_o8[1],new T(function(){return B(_nI(_o8[2],_nK));})];default:return new F(function(){return _nL(_);});}},_ob=[0,41],_oc=[1,_ob,_1g],_od=[0,40],_oe=[1,_od,_1g],_of=function(_og,_oh){return E(_og)[1]!=E(_oh)[1];},_oi=function(_oj,_ok){return E(_oj)[1]==E(_ok)[1];},_ol=[0,_oi,_of],_om=function(_on,_oo){while(1){var _op=E(_on);if(!_op[0]){return E(_oo)[0]==0?true:false;}else{var _oq=E(_oo);if(!_oq[0]){return false;}else{if(E(_op[1])[1]!=E(_oq[1])[1]){return false;}else{_on=_op[2];_oo=_oq[2];continue;}}}}},_or=function(_os,_ot){return !B(_om(_os,_ot))?true:false;},_ou=[0,_om,_or],_ov=function(_ow,_ox){var _oy=E(_ow);switch(_oy[0]){case 0:return [0,function(_oz){return new F(function(){return _ov(B(A(_oy[1],[_oz])),_ox);});}];case 1:return [1,function(_oA){return new F(function(){return _ov(B(A(_oy[1],[_oA])),_ox);});}];case 2:return [2];case 3:return new F(function(){return _nI(B(A(_ox,[_oy[1]])),new T(function(){return B(_ov(_oy[2],_ox));}));});break;default:var _oB=function(_oC){var _oD=E(_oC);if(!_oD[0]){return [0];}else{var _oE=E(_oD[1]);return new F(function(){return _5B(B(_ny(B(A(_ox,[_oE[1]])),_oE[2])),new T(function(){return B(_oB(_oD[2]));},1));});}},_oF=B(_oB(_oy[1]));return _oF[0]==0?[2]:[4,_oF];}},_oG=[2],_oH=function(_oI){return [3,_oI,_oG];},_oJ=function(_oK,_oL){var _oM=E(_oK);if(!_oM){return new F(function(){return A(_oL,[_c]);});}else{return [0,function(_oN){return E(new T(function(){return B(_oJ(_oM-1|0,_oL));}));}];}},_oO=function(_oP,_oQ,_oR){return function(_oS){return new F(function(){return A(function(_oT,_oU,_oV){while(1){var _oW=(function(_oX,_oY,_oZ){var _p0=E(_oX);switch(_p0[0]){case 0:var _p1=E(_oY);if(!_p1[0]){return E(_oQ);}else{_oT=B(A(_p0[1],[_p1[1]]));_oU=_p1[2];var _p2=_oZ+1|0;_oV=_p2;return null;}break;case 1:var _p3=B(A(_p0[1],[_oY])),_p4=_oY,_p2=_oZ;_oT=_p3;_oU=_p4;_oV=_p2;return null;case 2:return E(_oQ);case 3:return function(_p5){return new F(function(){return _oJ(_oZ,function(_p6){return E(new T(function(){return B(_ov(_p0,_p5));}));});});};default:return function(_b3){return new F(function(){return _ov(_p0,_b3);});};}})(_oT,_oU,_oV);if(_oW!=null){return _oW;}}},[new T(function(){return B(A(_oP,[_oH]));}),_oS,0,_oR]);});};},_p7=function(_p8){return new F(function(){return A(_p8,[_1g]);});},_p9=function(_pa,_pb){var _pc=function(_pd){var _pe=E(_pd);if(!_pe[0]){return E(_p7);}else{var _pf=_pe[1];return !B(A(_pa,[_pf]))?E(_p7):function(_pg){return [0,function(_ph){return E(new T(function(){return B(A(new T(function(){return B(_pc(_pe[2]));}),[function(_pi){return new F(function(){return A(_pg,[[1,_pf,_pi]]);});}]));}));}];};}};return function(_pj){return new F(function(){return A(_pc,[_pj,_pb]);});};},_pk=[6],_pl=new T(function(){return B(unCStr("valDig: Bad base"));}),_pm=new T(function(){return B(err(_pl));}),_pn=function(_po,_pp){var _pq=function(_pr,_ps){var _pt=E(_pr);if(!_pt[0]){return function(_pu){return new F(function(){return A(_pu,[new T(function(){return B(A(_ps,[_1g]));})]);});};}else{var _pv=E(_pt[1])[1],_pw=function(_px){return function(_py){return [0,function(_pz){return E(new T(function(){return B(A(new T(function(){return B(_pq(_pt[2],function(_pA){return new F(function(){return A(_ps,[[1,_px,_pA]]);});}));}),[_py]));}));}];};};switch(E(E(_po)[1])){case 8:if(48>_pv){return function(_pB){return new F(function(){return A(_pB,[new T(function(){return B(A(_ps,[_1g]));})]);});};}else{if(_pv>55){return function(_pC){return new F(function(){return A(_pC,[new T(function(){return B(A(_ps,[_1g]));})]);});};}else{return new F(function(){return _pw([0,_pv-48|0]);});}}break;case 10:if(48>_pv){return function(_pD){return new F(function(){return A(_pD,[new T(function(){return B(A(_ps,[_1g]));})]);});};}else{if(_pv>57){return function(_pE){return new F(function(){return A(_pE,[new T(function(){return B(A(_ps,[_1g]));})]);});};}else{return new F(function(){return _pw([0,_pv-48|0]);});}}break;case 16:if(48>_pv){if(97>_pv){if(65>_pv){return function(_pF){return new F(function(){return A(_pF,[new T(function(){return B(A(_ps,[_1g]));})]);});};}else{if(_pv>70){return function(_pG){return new F(function(){return A(_pG,[new T(function(){return B(A(_ps,[_1g]));})]);});};}else{return new F(function(){return _pw([0,(_pv-65|0)+10|0]);});}}}else{if(_pv>102){if(65>_pv){return function(_pH){return new F(function(){return A(_pH,[new T(function(){return B(A(_ps,[_1g]));})]);});};}else{if(_pv>70){return function(_pI){return new F(function(){return A(_pI,[new T(function(){return B(A(_ps,[_1g]));})]);});};}else{return new F(function(){return _pw([0,(_pv-65|0)+10|0]);});}}}else{return new F(function(){return _pw([0,(_pv-97|0)+10|0]);});}}}else{if(_pv>57){if(97>_pv){if(65>_pv){return function(_pJ){return new F(function(){return A(_pJ,[new T(function(){return B(A(_ps,[_1g]));})]);});};}else{if(_pv>70){return function(_pK){return new F(function(){return A(_pK,[new T(function(){return B(A(_ps,[_1g]));})]);});};}else{return new F(function(){return _pw([0,(_pv-65|0)+10|0]);});}}}else{if(_pv>102){if(65>_pv){return function(_pL){return new F(function(){return A(_pL,[new T(function(){return B(A(_ps,[_1g]));})]);});};}else{if(_pv>70){return function(_pM){return new F(function(){return A(_pM,[new T(function(){return B(A(_ps,[_1g]));})]);});};}else{return new F(function(){return _pw([0,(_pv-65|0)+10|0]);});}}}else{return new F(function(){return _pw([0,(_pv-97|0)+10|0]);});}}}else{return new F(function(){return _pw([0,_pv-48|0]);});}}break;default:return E(_pm);}}};return function(_pN){return new F(function(){return A(_pq,[_pN,_t,function(_pO){var _pP=E(_pO);return _pP[0]==0?[2]:B(A(_pp,[_pP]));}]);});};},_pQ=[0,10],_pR=[0,1],_pS=[0,2147483647],_pT=function(_pU,_pV){while(1){var _pW=E(_pU);if(!_pW[0]){var _pX=_pW[1],_pY=E(_pV);if(!_pY[0]){var _pZ=_pY[1],_q0=addC(_pX,_pZ);if(!E(_q0[2])){return [0,_q0[1]];}else{_pU=[1,I_fromInt(_pX)];_pV=[1,I_fromInt(_pZ)];continue;}}else{_pU=[1,I_fromInt(_pX)];_pV=_pY;continue;}}else{var _q1=E(_pV);if(!_q1[0]){_pU=_pW;_pV=[1,I_fromInt(_q1[1])];continue;}else{return [1,I_add(_pW[1],_q1[1])];}}}},_q2=new T(function(){return B(_pT(_pS,_pR));}),_q3=function(_q4){var _q5=E(_q4);if(!_q5[0]){var _q6=E(_q5[1]);return _q6==(-2147483648)?E(_q2):[0, -_q6];}else{return [1,I_negate(_q5[1])];}},_q7=[0,10],_q8=[0,0],_q9=function(_qa){return [0,_qa];},_qb=function(_qc,_qd){while(1){var _qe=E(_qc);if(!_qe[0]){var _qf=_qe[1],_qg=E(_qd);if(!_qg[0]){var _qh=_qg[1];if(!(imul(_qf,_qh)|0)){return [0,imul(_qf,_qh)|0];}else{_qc=[1,I_fromInt(_qf)];_qd=[1,I_fromInt(_qh)];continue;}}else{_qc=[1,I_fromInt(_qf)];_qd=_qg;continue;}}else{var _qi=E(_qd);if(!_qi[0]){_qc=_qe;_qd=[1,I_fromInt(_qi[1])];continue;}else{return [1,I_mul(_qe[1],_qi[1])];}}}},_qj=function(_qk,_ql,_qm){while(1){var _qn=E(_qm);if(!_qn[0]){return E(_ql);}else{var _qo=B(_pT(B(_qb(_ql,_qk)),B(_q9(E(_qn[1])[1]))));_qm=_qn[2];_ql=_qo;continue;}}},_qp=function(_qq){var _qr=new T(function(){return B(_nI(B(_nI([0,function(_qs){return E(E(_qs)[1])==45?[1,B(_pn(_pQ,function(_qt){return new F(function(){return A(_qq,[[1,new T(function(){return B(_q3(B(_qj(_q7,_q8,_qt))));})]]);});}))]:[2];}],[0,function(_qu){return E(E(_qu)[1])==43?[1,B(_pn(_pQ,function(_qv){return new F(function(){return A(_qq,[[1,new T(function(){return B(_qj(_q7,_q8,_qv));})]]);});}))]:[2];}])),new T(function(){return [1,B(_pn(_pQ,function(_qw){return new F(function(){return A(_qq,[[1,new T(function(){return B(_qj(_q7,_q8,_qw));})]]);});}))];})));});return new F(function(){return _nI([0,function(_qx){return E(E(_qx)[1])==101?E(_qr):[2];}],[0,function(_qy){return E(E(_qy)[1])==69?E(_qr):[2];}]);});},_qz=function(_qA){return new F(function(){return A(_qA,[_5A]);});},_qB=function(_qC){return new F(function(){return A(_qC,[_5A]);});},_qD=function(_qE){return function(_qF){return E(E(_qF)[1])==46?[1,B(_pn(_pQ,function(_qG){return new F(function(){return A(_qE,[[1,_qG]]);});}))]:[2];};},_qH=function(_qI){return [0,B(_qD(_qI))];},_qJ=function(_qK){return new F(function(){return _pn(_pQ,function(_qL){return [1,B(_oO(_qH,_qz,function(_qM){return [1,B(_oO(_qp,_qB,function(_qN){return new F(function(){return A(_qK,[[5,[1,_qL,_qM,_qN]]]);});}))];}))];});});},_qO=function(_qP){return [1,B(_qJ(_qP))];},_qQ=function(_qR){return E(E(_qR)[1]);},_qS=function(_qT,_qU,_qV){while(1){var _qW=E(_qV);if(!_qW[0]){return false;}else{if(!B(A(_qQ,[_qT,_qU,_qW[1]]))){_qV=_qW[2];continue;}else{return true;}}}},_qX=new T(function(){return B(unCStr("!@#$%&*+./<=>?\\^|:-~"));}),_qY=function(_qZ){return new F(function(){return _qS(_ol,_qZ,_qX);});},_r0=[0,8],_r1=[0,16],_r2=function(_r3){var _r4=function(_r5){return new F(function(){return A(_r3,[[5,[0,_r0,_r5]]]);});},_r6=function(_r7){return new F(function(){return A(_r3,[[5,[0,_r1,_r7]]]);});};return function(_r8){return E(E(_r8)[1])==48?E([0,function(_r9){switch(E(E(_r9)[1])){case 79:return [1,B(_pn(_r0,_r4))];case 88:return [1,B(_pn(_r1,_r6))];case 111:return [1,B(_pn(_r0,_r4))];case 120:return [1,B(_pn(_r1,_r6))];default:return [2];}}]):[2];};},_ra=function(_rb){return [0,B(_r2(_rb))];},_rc=function(_rd){var _re=new T(function(){return B(A(_rd,[_r0]));}),_rf=new T(function(){return B(A(_rd,[_r1]));});return function(_rg){switch(E(E(_rg)[1])){case 79:return E(_re);case 88:return E(_rf);case 111:return E(_re);case 120:return E(_rf);default:return [2];}};},_rh=function(_ri){return [0,B(_rc(_ri))];},_rj=[0,92],_rk=function(_rl){return new F(function(){return A(_rl,[_pQ]);});},_rm=function(_rn){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_5M(9,_rn,_1g));}))));});},_ro=function(_rp){var _rq=E(_rp);return _rq[0]==0?E(_rq[1]):I_toInt(_rq[1]);},_rr=function(_rs,_rt){var _ru=E(_rs);if(!_ru[0]){var _rv=_ru[1],_rw=E(_rt);return _rw[0]==0?_rv<=_rw[1]:I_compareInt(_rw[1],_rv)>=0;}else{var _rx=_ru[1],_ry=E(_rt);return _ry[0]==0?I_compareInt(_rx,_ry[1])<=0:I_compare(_rx,_ry[1])<=0;}},_rz=function(_rA){return [2];},_rB=function(_rC){var _rD=E(_rC);if(!_rD[0]){return E(_rz);}else{var _rE=_rD[1],_rF=E(_rD[2]);return _rF[0]==0?E(_rE):function(_rG){return new F(function(){return _nI(B(A(_rE,[_rG])),new T(function(){return B(A(new T(function(){return B(_rB(_rF));}),[_rG]));}));});};}},_rH=function(_rI){return [2];},_rJ=function(_rK,_rL){var _rM=function(_rN,_rO){var _rP=E(_rN);if(!_rP[0]){return function(_rQ){return new F(function(){return A(_rQ,[_rK]);});};}else{var _rR=E(_rO);return _rR[0]==0?E(_rH):E(_rP[1])[1]!=E(_rR[1])[1]?E(_rH):function(_rS){return [0,function(_rT){return E(new T(function(){return B(A(new T(function(){return B(_rM(_rP[2],_rR[2]));}),[_rS]));}));}];};}};return function(_rU){return new F(function(){return A(_rM,[_rK,_rU,_rL]);});};},_rV=new T(function(){return B(unCStr("SOH"));}),_rW=[0,1],_rX=function(_rY){return [1,B(_rJ(_rV,function(_rZ){return E(new T(function(){return B(A(_rY,[_rW]));}));}))];},_s0=new T(function(){return B(unCStr("SO"));}),_s1=[0,14],_s2=function(_s3){return [1,B(_rJ(_s0,function(_s4){return E(new T(function(){return B(A(_s3,[_s1]));}));}))];},_s5=function(_s6){return [1,B(_oO(_rX,_s2,_s6))];},_s7=new T(function(){return B(unCStr("NUL"));}),_s8=[0,0],_s9=function(_sa){return [1,B(_rJ(_s7,function(_sb){return E(new T(function(){return B(A(_sa,[_s8]));}));}))];},_sc=new T(function(){return B(unCStr("STX"));}),_sd=[0,2],_se=function(_sf){return [1,B(_rJ(_sc,function(_sg){return E(new T(function(){return B(A(_sf,[_sd]));}));}))];},_sh=new T(function(){return B(unCStr("ETX"));}),_si=[0,3],_sj=function(_sk){return [1,B(_rJ(_sh,function(_sl){return E(new T(function(){return B(A(_sk,[_si]));}));}))];},_sm=new T(function(){return B(unCStr("EOT"));}),_sn=[0,4],_so=function(_sp){return [1,B(_rJ(_sm,function(_sq){return E(new T(function(){return B(A(_sp,[_sn]));}));}))];},_sr=new T(function(){return B(unCStr("ENQ"));}),_ss=[0,5],_st=function(_su){return [1,B(_rJ(_sr,function(_sv){return E(new T(function(){return B(A(_su,[_ss]));}));}))];},_sw=new T(function(){return B(unCStr("ACK"));}),_sx=[0,6],_sy=function(_sz){return [1,B(_rJ(_sw,function(_sA){return E(new T(function(){return B(A(_sz,[_sx]));}));}))];},_sB=new T(function(){return B(unCStr("BEL"));}),_sC=[0,7],_sD=function(_sE){return [1,B(_rJ(_sB,function(_sF){return E(new T(function(){return B(A(_sE,[_sC]));}));}))];},_sG=new T(function(){return B(unCStr("BS"));}),_sH=[0,8],_sI=function(_sJ){return [1,B(_rJ(_sG,function(_sK){return E(new T(function(){return B(A(_sJ,[_sH]));}));}))];},_sL=new T(function(){return B(unCStr("HT"));}),_sM=[0,9],_sN=function(_sO){return [1,B(_rJ(_sL,function(_sP){return E(new T(function(){return B(A(_sO,[_sM]));}));}))];},_sQ=new T(function(){return B(unCStr("LF"));}),_sR=[0,10],_sS=function(_sT){return [1,B(_rJ(_sQ,function(_sU){return E(new T(function(){return B(A(_sT,[_sR]));}));}))];},_sV=new T(function(){return B(unCStr("VT"));}),_sW=[0,11],_sX=function(_sY){return [1,B(_rJ(_sV,function(_sZ){return E(new T(function(){return B(A(_sY,[_sW]));}));}))];},_t0=new T(function(){return B(unCStr("FF"));}),_t1=[0,12],_t2=function(_t3){return [1,B(_rJ(_t0,function(_t4){return E(new T(function(){return B(A(_t3,[_t1]));}));}))];},_t5=new T(function(){return B(unCStr("CR"));}),_t6=[0,13],_t7=function(_t8){return [1,B(_rJ(_t5,function(_t9){return E(new T(function(){return B(A(_t8,[_t6]));}));}))];},_ta=new T(function(){return B(unCStr("SI"));}),_tb=[0,15],_tc=function(_td){return [1,B(_rJ(_ta,function(_te){return E(new T(function(){return B(A(_td,[_tb]));}));}))];},_tf=new T(function(){return B(unCStr("DLE"));}),_tg=[0,16],_th=function(_ti){return [1,B(_rJ(_tf,function(_tj){return E(new T(function(){return B(A(_ti,[_tg]));}));}))];},_tk=new T(function(){return B(unCStr("DC1"));}),_tl=[0,17],_tm=function(_tn){return [1,B(_rJ(_tk,function(_to){return E(new T(function(){return B(A(_tn,[_tl]));}));}))];},_tp=new T(function(){return B(unCStr("DC2"));}),_tq=[0,18],_tr=function(_ts){return [1,B(_rJ(_tp,function(_tt){return E(new T(function(){return B(A(_ts,[_tq]));}));}))];},_tu=new T(function(){return B(unCStr("DC3"));}),_tv=[0,19],_tw=function(_tx){return [1,B(_rJ(_tu,function(_ty){return E(new T(function(){return B(A(_tx,[_tv]));}));}))];},_tz=new T(function(){return B(unCStr("DC4"));}),_tA=[0,20],_tB=function(_tC){return [1,B(_rJ(_tz,function(_tD){return E(new T(function(){return B(A(_tC,[_tA]));}));}))];},_tE=new T(function(){return B(unCStr("NAK"));}),_tF=[0,21],_tG=function(_tH){return [1,B(_rJ(_tE,function(_tI){return E(new T(function(){return B(A(_tH,[_tF]));}));}))];},_tJ=new T(function(){return B(unCStr("SYN"));}),_tK=[0,22],_tL=function(_tM){return [1,B(_rJ(_tJ,function(_tN){return E(new T(function(){return B(A(_tM,[_tK]));}));}))];},_tO=new T(function(){return B(unCStr("ETB"));}),_tP=[0,23],_tQ=function(_tR){return [1,B(_rJ(_tO,function(_tS){return E(new T(function(){return B(A(_tR,[_tP]));}));}))];},_tT=new T(function(){return B(unCStr("CAN"));}),_tU=[0,24],_tV=function(_tW){return [1,B(_rJ(_tT,function(_tX){return E(new T(function(){return B(A(_tW,[_tU]));}));}))];},_tY=new T(function(){return B(unCStr("EM"));}),_tZ=[0,25],_u0=function(_u1){return [1,B(_rJ(_tY,function(_u2){return E(new T(function(){return B(A(_u1,[_tZ]));}));}))];},_u3=new T(function(){return B(unCStr("SUB"));}),_u4=[0,26],_u5=function(_u6){return [1,B(_rJ(_u3,function(_u7){return E(new T(function(){return B(A(_u6,[_u4]));}));}))];},_u8=new T(function(){return B(unCStr("ESC"));}),_u9=[0,27],_ua=function(_ub){return [1,B(_rJ(_u8,function(_uc){return E(new T(function(){return B(A(_ub,[_u9]));}));}))];},_ud=new T(function(){return B(unCStr("FS"));}),_ue=[0,28],_uf=function(_ug){return [1,B(_rJ(_ud,function(_uh){return E(new T(function(){return B(A(_ug,[_ue]));}));}))];},_ui=new T(function(){return B(unCStr("GS"));}),_uj=[0,29],_uk=function(_ul){return [1,B(_rJ(_ui,function(_um){return E(new T(function(){return B(A(_ul,[_uj]));}));}))];},_un=new T(function(){return B(unCStr("RS"));}),_uo=[0,30],_up=function(_uq){return [1,B(_rJ(_un,function(_ur){return E(new T(function(){return B(A(_uq,[_uo]));}));}))];},_us=new T(function(){return B(unCStr("US"));}),_ut=[0,31],_uu=function(_uv){return [1,B(_rJ(_us,function(_uw){return E(new T(function(){return B(A(_uv,[_ut]));}));}))];},_ux=new T(function(){return B(unCStr("SP"));}),_uy=[0,32],_uz=function(_uA){return [1,B(_rJ(_ux,function(_uB){return E(new T(function(){return B(A(_uA,[_uy]));}));}))];},_uC=new T(function(){return B(unCStr("DEL"));}),_uD=[0,127],_uE=function(_uF){return [1,B(_rJ(_uC,function(_uG){return E(new T(function(){return B(A(_uF,[_uD]));}));}))];},_uH=[1,_uE,_1g],_uI=[1,_uz,_uH],_uJ=[1,_uu,_uI],_uK=[1,_up,_uJ],_uL=[1,_uk,_uK],_uM=[1,_uf,_uL],_uN=[1,_ua,_uM],_uO=[1,_u5,_uN],_uP=[1,_u0,_uO],_uQ=[1,_tV,_uP],_uR=[1,_tQ,_uQ],_uS=[1,_tL,_uR],_uT=[1,_tG,_uS],_uU=[1,_tB,_uT],_uV=[1,_tw,_uU],_uW=[1,_tr,_uV],_uX=[1,_tm,_uW],_uY=[1,_th,_uX],_uZ=[1,_tc,_uY],_v0=[1,_t7,_uZ],_v1=[1,_t2,_v0],_v2=[1,_sX,_v1],_v3=[1,_sS,_v2],_v4=[1,_sN,_v3],_v5=[1,_sI,_v4],_v6=[1,_sD,_v5],_v7=[1,_sy,_v6],_v8=[1,_st,_v7],_v9=[1,_so,_v8],_va=[1,_sj,_v9],_vb=[1,_se,_va],_vc=[1,_s9,_vb],_vd=[1,_s5,_vc],_ve=new T(function(){return B(_rB(_vd));}),_vf=[0,1114111],_vg=[0,34],_vh=[0,39],_vi=function(_vj){var _vk=new T(function(){return B(A(_vj,[_sC]));}),_vl=new T(function(){return B(A(_vj,[_sH]));}),_vm=new T(function(){return B(A(_vj,[_sM]));}),_vn=new T(function(){return B(A(_vj,[_sR]));}),_vo=new T(function(){return B(A(_vj,[_sW]));}),_vp=new T(function(){return B(A(_vj,[_t1]));}),_vq=new T(function(){return B(A(_vj,[_t6]));});return new F(function(){return _nI([0,function(_vr){switch(E(E(_vr)[1])){case 34:return E(new T(function(){return B(A(_vj,[_vg]));}));case 39:return E(new T(function(){return B(A(_vj,[_vh]));}));case 92:return E(new T(function(){return B(A(_vj,[_rj]));}));case 97:return E(_vk);case 98:return E(_vl);case 102:return E(_vp);case 110:return E(_vn);case 114:return E(_vq);case 116:return E(_vm);case 118:return E(_vo);default:return [2];}}],new T(function(){return B(_nI([1,B(_oO(_rh,_rk,function(_vs){return [1,B(_pn(_vs,function(_vt){var _vu=B(_qj(new T(function(){return B(_q9(E(_vs)[1]));}),_q8,_vt));return !B(_rr(_vu,_vf))?[2]:B(A(_vj,[new T(function(){var _vv=B(_ro(_vu));if(_vv>>>0>1114111){var _vw=B(_rm(_vv));}else{var _vw=[0,_vv];}var _vx=_vw,_vy=_vx,_vz=_vy;return _vz;})]));}))];}))],new T(function(){return B(_nI([0,function(_vA){return E(E(_vA)[1])==94?E([0,function(_vB){switch(E(E(_vB)[1])){case 64:return E(new T(function(){return B(A(_vj,[_s8]));}));case 65:return E(new T(function(){return B(A(_vj,[_rW]));}));case 66:return E(new T(function(){return B(A(_vj,[_sd]));}));case 67:return E(new T(function(){return B(A(_vj,[_si]));}));case 68:return E(new T(function(){return B(A(_vj,[_sn]));}));case 69:return E(new T(function(){return B(A(_vj,[_ss]));}));case 70:return E(new T(function(){return B(A(_vj,[_sx]));}));case 71:return E(_vk);case 72:return E(_vl);case 73:return E(_vm);case 74:return E(_vn);case 75:return E(_vo);case 76:return E(_vp);case 77:return E(_vq);case 78:return E(new T(function(){return B(A(_vj,[_s1]));}));case 79:return E(new T(function(){return B(A(_vj,[_tb]));}));case 80:return E(new T(function(){return B(A(_vj,[_tg]));}));case 81:return E(new T(function(){return B(A(_vj,[_tl]));}));case 82:return E(new T(function(){return B(A(_vj,[_tq]));}));case 83:return E(new T(function(){return B(A(_vj,[_tv]));}));case 84:return E(new T(function(){return B(A(_vj,[_tA]));}));case 85:return E(new T(function(){return B(A(_vj,[_tF]));}));case 86:return E(new T(function(){return B(A(_vj,[_tK]));}));case 87:return E(new T(function(){return B(A(_vj,[_tP]));}));case 88:return E(new T(function(){return B(A(_vj,[_tU]));}));case 89:return E(new T(function(){return B(A(_vj,[_tZ]));}));case 90:return E(new T(function(){return B(A(_vj,[_u4]));}));case 91:return E(new T(function(){return B(A(_vj,[_u9]));}));case 92:return E(new T(function(){return B(A(_vj,[_ue]));}));case 93:return E(new T(function(){return B(A(_vj,[_uj]));}));case 94:return E(new T(function(){return B(A(_vj,[_uo]));}));case 95:return E(new T(function(){return B(A(_vj,[_ut]));}));default:return [2];}}]):[2];}],new T(function(){return B(A(_ve,[_vj]));})));})));}));});},_vC=function(_vD){return new F(function(){return A(_vD,[_c]);});},_vE=function(_vF){var _vG=E(_vF);if(!_vG[0]){return E(_vC);}else{var _vH=_vG[2],_vI=E(E(_vG[1])[1]);switch(_vI){case 9:return function(_vJ){return [0,function(_vK){return E(new T(function(){return B(A(new T(function(){return B(_vE(_vH));}),[_vJ]));}));}];};case 10:return function(_vL){return [0,function(_vM){return E(new T(function(){return B(A(new T(function(){return B(_vE(_vH));}),[_vL]));}));}];};case 11:return function(_vN){return [0,function(_vO){return E(new T(function(){return B(A(new T(function(){return B(_vE(_vH));}),[_vN]));}));}];};case 12:return function(_vP){return [0,function(_vQ){return E(new T(function(){return B(A(new T(function(){return B(_vE(_vH));}),[_vP]));}));}];};case 13:return function(_vR){return [0,function(_vS){return E(new T(function(){return B(A(new T(function(){return B(_vE(_vH));}),[_vR]));}));}];};case 32:return function(_vT){return [0,function(_vU){return E(new T(function(){return B(A(new T(function(){return B(_vE(_vH));}),[_vT]));}));}];};case 160:return function(_vV){return [0,function(_vW){return E(new T(function(){return B(A(new T(function(){return B(_vE(_vH));}),[_vV]));}));}];};default:var _vX=u_iswspace(_vI),_vY=_vX;return E(_vY)==0?E(_vC):function(_vZ){return [0,function(_w0){return E(new T(function(){return B(A(new T(function(){return B(_vE(_vH));}),[_vZ]));}));}];};}}},_w1=function(_w2){var _w3=new T(function(){return B(_w1(_w2));}),_w4=[1,function(_w5){return new F(function(){return A(_vE,[_w5,function(_w6){return E([0,function(_w7){return E(E(_w7)[1])==92?E(_w3):[2];}]);}]);});}];return new F(function(){return _nI([0,function(_w8){return E(E(_w8)[1])==92?E([0,function(_w9){var _wa=E(E(_w9)[1]);switch(_wa){case 9:return E(_w4);case 10:return E(_w4);case 11:return E(_w4);case 12:return E(_w4);case 13:return E(_w4);case 32:return E(_w4);case 38:return E(_w3);case 160:return E(_w4);default:var _wb=u_iswspace(_wa),_wc=_wb;return E(_wc)==0?[2]:E(_w4);}}]):[2];}],[0,function(_wd){var _we=E(_wd);return E(_we[1])==92?E(new T(function(){return B(_vi(function(_wf){return new F(function(){return A(_w2,[[0,_wf,_fI]]);});}));})):B(A(_w2,[[0,_we,_61]]));}]);});},_wg=function(_wh,_wi){return new F(function(){return _w1(function(_wj){var _wk=E(_wj),_wl=E(_wk[1]);if(E(_wl[1])==34){if(!E(_wk[2])){return E(new T(function(){return B(A(_wi,[[1,new T(function(){return B(A(_wh,[_1g]));})]]));}));}else{return new F(function(){return _wg(function(_wm){return new F(function(){return A(_wh,[[1,_wl,_wm]]);});},_wi);});}}else{return new F(function(){return _wg(function(_wn){return new F(function(){return A(_wh,[[1,_wl,_wn]]);});},_wi);});}});});},_wo=new T(function(){return B(unCStr("_\'"));}),_wp=function(_wq){var _wr=u_iswalnum(_wq),_ws=_wr;return E(_ws)==0?B(_qS(_ol,[0,_wq],_wo)):true;},_wt=function(_wu){return new F(function(){return _wp(E(_wu)[1]);});},_wv=new T(function(){return B(unCStr(",;()[]{}`"));}),_ww=new T(function(){return B(unCStr(".."));}),_wx=new T(function(){return B(unCStr("::"));}),_wy=new T(function(){return B(unCStr("->"));}),_wz=[0,64],_wA=[1,_wz,_1g],_wB=[0,126],_wC=[1,_wB,_1g],_wD=new T(function(){return B(unCStr("=>"));}),_wE=[1,_wD,_1g],_wF=[1,_wC,_wE],_wG=[1,_wA,_wF],_wH=[1,_wy,_wG],_wI=new T(function(){return B(unCStr("<-"));}),_wJ=[1,_wI,_wH],_wK=[0,124],_wL=[1,_wK,_1g],_wM=[1,_wL,_wJ],_wN=[1,_rj,_1g],_wO=[1,_wN,_wM],_wP=[0,61],_wQ=[1,_wP,_1g],_wR=[1,_wQ,_wO],_wS=[1,_wx,_wR],_wT=[1,_ww,_wS],_wU=function(_wV){return new F(function(){return _nI([1,function(_wW){return E(_wW)[0]==0?E(new T(function(){return B(A(_wV,[_pk]));})):[2];}],new T(function(){return B(_nI([0,function(_wX){return E(E(_wX)[1])==39?E([0,function(_wY){var _wZ=E(_wY);switch(E(_wZ[1])){case 39:return [2];case 92:return E(new T(function(){return B(_vi(function(_x0){return [0,function(_x1){return E(E(_x1)[1])==39?E(new T(function(){return B(A(_wV,[[0,_x0]]));})):[2];}];}));}));default:return [0,function(_x2){return E(E(_x2)[1])==39?E(new T(function(){return B(A(_wV,[[0,_wZ]]));})):[2];}];}}]):[2];}],new T(function(){return B(_nI([0,function(_x3){return E(E(_x3)[1])==34?E(new T(function(){return B(_wg(_t,_wV));})):[2];}],new T(function(){return B(_nI([0,function(_x4){return !B(_qS(_ol,_x4,_wv))?[2]:B(A(_wV,[[2,[1,_x4,_1g]]]));}],new T(function(){return B(_nI([0,function(_x5){return !B(_qS(_ol,_x5,_qX))?[2]:[1,B(_p9(_qY,function(_x6){var _x7=[1,_x5,_x6];return !B(_qS(_ou,_x7,_wT))?B(A(_wV,[[4,_x7]])):B(A(_wV,[[2,_x7]]));}))];}],new T(function(){return B(_nI([0,function(_x8){var _x9=E(_x8),_xa=_x9[1],_xb=u_iswalpha(_xa),_xc=_xb;return E(_xc)==0?E(_xa)==95?[1,B(_p9(_wt,function(_xd){return new F(function(){return A(_wV,[[3,[1,_x9,_xd]]]);});}))]:[2]:[1,B(_p9(_wt,function(_xe){return new F(function(){return A(_wV,[[3,[1,_x9,_xe]]]);});}))];}],new T(function(){return [1,B(_oO(_ra,_qO,_wV))];})));})));})));})));})));}));});},_xf=[0,0],_xg=function(_xh,_xi){return function(_xj){return new F(function(){return A(_vE,[_xj,function(_xk){return E(new T(function(){return B(_wU(function(_xl){var _xm=E(_xl);return _xm[0]==2?!B(_h5(_xm[1],_oe))?[2]:E(new T(function(){return B(A(_xh,[_xf,function(_xn){return [1,function(_xo){return new F(function(){return A(_vE,[_xo,function(_xp){return E(new T(function(){return B(_wU(function(_xq){var _xr=E(_xq);return _xr[0]==2?!B(_h5(_xr[1],_oc))?[2]:E(new T(function(){return B(A(_xi,[_xn]));})):[2];}));}));}]);});}];}]));})):[2];}));}));}]);});};},_xs=function(_xt,_xu,_xv){var _xw=function(_xx,_xy){return new F(function(){return _nI([1,function(_xz){return new F(function(){return A(_vE,[_xz,function(_xA){return E(new T(function(){return B(_wU(function(_xB){var _xC=E(_xB);if(_xC[0]==4){var _xD=E(_xC[1]);if(!_xD[0]){return new F(function(){return A(_xt,[_xC,_xx,_xy]);});}else{return E(E(_xD[1])[1])==45?E(_xD[2])[0]==0?E([1,function(_xE){return new F(function(){return A(_vE,[_xE,function(_xF){return E(new T(function(){return B(_wU(function(_xG){return new F(function(){return A(_xt,[_xG,_xx,function(_xH){return new F(function(){return A(_xy,[new T(function(){return [0, -E(_xH)[1]];})]);});}]);});}));}));}]);});}]):B(A(_xt,[_xC,_xx,_xy])):B(A(_xt,[_xC,_xx,_xy]));}}else{return new F(function(){return A(_xt,[_xC,_xx,_xy]);});}}));}));}]);});}],new T(function(){return [1,B(_xg(_xw,_xy))];}));});};return new F(function(){return _xw(_xu,_xv);});},_xI=function(_xJ,_xK){return [2];},_xL=function(_xM){var _xN=E(_xM);return _xN[0]==0?[1,new T(function(){return B(_qj(new T(function(){return B(_q9(E(_xN[1])[1]));}),_q8,_xN[2]));})]:E(_xN[2])[0]==0?E(_xN[3])[0]==0?[1,new T(function(){return B(_qj(_q7,_q8,_xN[1]));})]:[0]:[0];},_xO=function(_xP){var _xQ=E(_xP);if(_xQ[0]==5){var _xR=B(_xL(_xQ[1]));return _xR[0]==0?E(_xI):function(_xS,_xT){return new F(function(){return A(_xT,[new T(function(){return [0,B(_ro(_xR[1]))];})]);});};}else{return E(_xI);}},_xU=function(_xV,_xW){return new F(function(){return _xs(_xO,_xV,_xW);});},_xX=[0,91],_xY=[1,_xX,_1g],_xZ=function(_y0,_y1){var _y2=function(_y3,_y4){return [1,function(_y5){return new F(function(){return A(_vE,[_y5,function(_y6){return E(new T(function(){return B(_wU(function(_y7){var _y8=E(_y7);if(_y8[0]==2){var _y9=E(_y8[1]);if(!_y9[0]){return [2];}else{var _ya=_y9[2];switch(E(E(_y9[1])[1])){case 44:return E(_ya)[0]==0?!E(_y3)?[2]:E(new T(function(){return B(A(_y0,[_xf,function(_yb){return new F(function(){return _y2(_fI,function(_yc){return new F(function(){return A(_y4,[[1,_yb,_yc]]);});});});}]));})):[2];case 93:return E(_ya)[0]==0?E(new T(function(){return B(A(_y4,[_1g]));})):[2];default:return [2];}}}else{return [2];}}));}));}]);});}];},_yd=function(_ye){return new F(function(){return _nI([1,function(_yf){return new F(function(){return A(_vE,[_yf,function(_yg){return E(new T(function(){return B(_wU(function(_yh){var _yi=E(_yh);return _yi[0]==2?!B(_h5(_yi[1],_xY))?[2]:E(new T(function(){return B(_nI(B(_y2(_61,_ye)),new T(function(){return B(A(_y0,[_xf,function(_yj){return new F(function(){return _y2(_fI,function(_yk){return new F(function(){return A(_ye,[[1,_yj,_yk]]);});});});}]));})));})):[2];}));}));}]);});}],new T(function(){return [1,B(_xg(function(_yl,_ym){return new F(function(){return _yd(_ym);});},_ye))];}));});};return new F(function(){return _yd(_y1);});},_yn=function(_yo,_yp){return new F(function(){return _xZ(_xU,_yp);});},_yq=function(_yr){return function(_b3){return new F(function(){return _ny(new T(function(){return B(_xs(_xO,_yr,_oH));}),_b3);});};},_ys=new T(function(){return B(_xZ(_xU,_oH));}),_yt=function(_xW){return new F(function(){return _ny(_ys,_xW);});},_yu=[0,_yq,_yt,_xU,_yn],_yv=function(_yw){return new F(function(){return _5M(0,E(_yw)[1],_1g);});},_yx=function(_yy,_yz){return new F(function(){return _5M(0,E(_yy)[1],_yz);});},_yA=function(_yB,_yC){return new F(function(){return _dr(_yx,_yB,_yC);});},_yD=function(_yE,_yF,_yG){return new F(function(){return _5M(E(_yE)[1],E(_yF)[1],_yG);});},_yH=[0,_yD,_yv,_yA],_yI=new T(function(){return B(unCStr("GHC.Types"));}),_yJ=new T(function(){return B(unCStr("Int"));}),_yK=new T(function(){var _yL=hs_wordToWord64(1521842780),_yM=_yL,_yN=hs_wordToWord64(1346191152),_yO=_yN;return [0,_yM,_yO,[0,_yM,_yO,_iN,_yI,_yJ],_1g];}),_yP=function(_yQ){return E(_yK);},_yR=function(_yS){return E(E(_yS)[1]);},_yT=function(_yU){return E(E(_yU)[1]);},_yV=function(_yW){return E(E(_yW)[2]);},_yX=function(_yY){return E(E(_yY)[3]);},_yZ=function(_z0,_z1){var _z2=new T(function(){return B(_yR(_z0));});return function(_z3){return new F(function(){return A(new T(function(){return B(_yT(_z2));}),[new T(function(){return B(A(_yV,[_z0,_z1]));}),function(_z4){return new F(function(){return A(new T(function(){return B(_yX(_z2));}),[[0,_z4,_z3]]);});}]);});};},_z5=function(_z6,_z7){return [0,_z6,function(_z8){return new F(function(){return _yZ(_z7,_z8);});}];},_z9=function(_za,_zb,_zc,_zd){return new F(function(){return A(_yT,[_za,new T(function(){return B(A(_zb,[_zd]));}),function(_ze){return new F(function(){return A(_zc,[new T(function(){return E(E(_ze)[1]);}),new T(function(){return E(E(_ze)[2]);})]);});}]);});},_zf=function(_zg,_zh,_zi,_zj){return new F(function(){return A(_yT,[_zg,new T(function(){return B(A(_zh,[_zj]));}),function(_zk){return new F(function(){return A(_zi,[new T(function(){return E(E(_zk)[2]);})]);});}]);});},_zl=function(_zm,_zn,_zo,_zp){return new F(function(){return _zf(_zm,_zn,_zo,_zp);});},_zq=function(_zr){return E(E(_zr)[4]);},_zs=function(_zt,_zu){return function(_zv){return E(new T(function(){return B(A(_zq,[_zt,_zu]));}));};},_zw=function(_zx){return [0,function(_zn,_zo,_zp){return new F(function(){return _z9(_zx,_zn,_zo,_zp);});},function(_zn,_zo,_zp){return new F(function(){return _zl(_zx,_zn,_zo,_zp);});},function(_zy,_zz){return new F(function(){return A(new T(function(){return B(_yX(_zx));}),[[0,_zy,_zz]]);});},function(_zp){return new F(function(){return _zs(_zx,_zp);});}];},_zA=function(_zB,_zC,_zD){return new F(function(){return A(_yX,[_zB,[0,_zC,_zD]]);});},_zE=function(_zF){return E(E(_zF)[1]);},_zG=[0,10],_zH=function(_zI,_zJ){var _zK=E(_zJ);if(!_zK[0]){return E(_t);}else{var _zL=_zK[1],_zM=E(_zK[2]);if(!_zM[0]){var _zN=E(_zL);return new F(function(){return _zO(_zG,_zN[3],_zN[4]);});}else{return function(_zP){return new F(function(){return A(new T(function(){var _zQ=E(_zL);return B(_zO(_zG,_zQ[3],_zQ[4]));}),[new T(function(){return B(A(_zI,[new T(function(){return B(A(new T(function(){return B(_zH(_zI,_zM));}),[_zP]));})]));})]);});};}}},_zR=new T(function(){return B(unCStr("(->)"));}),_zS=new T(function(){return B(unCStr("GHC.Prim"));}),_zT=new T(function(){var _zU=hs_wordToWord64(4173248105),_zV=_zU,_zW=hs_wordToWord64(4270398258),_zX=_zW;return [0,_zV,_zX,[0,_zV,_zX,_iN,_zS,_zR],_1g];}),_zY=new T(function(){return E(E(_zT)[3]);}),_zZ=new T(function(){return B(unCStr("[]"));}),_A0=new T(function(){var _A1=hs_wordToWord64(4033920485),_A2=_A1,_A3=hs_wordToWord64(786266835),_A4=_A3;return [0,_A2,_A4,[0,_A2,_A4,_iN,_yI,_zZ],_1g];}),_A5=[1,_iO,_1g],_A6=function(_A7){var _A8=E(_A7);if(!_A8[0]){return [0];}else{var _A9=E(_A8[1]);return [1,[0,_A9[1],_A9[2]],new T(function(){return B(_A6(_A8[2]));})];}},_Aa=new T(function(){var _Ab=E(_A0),_Ac=E(_Ab[3]),_Ad=B(_5B(_Ab[4],_A5));if(!_Ad[0]){var _Ae=E(_Ac);}else{var _Af=B(_ji(new T(function(){return B(_jA(B(_8X(_jO,[1,[0,_Ac[1],_Ac[2]],new T(function(){return B(_A6(_Ad));})]))));},1))),_Ae=E(_Ac);}var _Ag=_Ae,_Ah=_Ag;return _Ah;}),_Ai=[0,8],_Aj=[0,32],_Ak=function(_Al){return [1,_Aj,_Al];},_Am=new T(function(){return B(unCStr(" -> "));}),_An=[0,9],_Ao=[0,93],_Ap=[0,91],_Aq=[0,41],_Ar=[0,44],_As=function(_Al){return [1,_Ar,_Al];},_At=[0,40],_Au=[0,0],_zO=function(_Av,_Aw,_Ax){var _Ay=E(_Ax);if(!_Ay[0]){return function(_Az){return new F(function(){return _5B(E(_Aw)[5],_Az);});};}else{var _AA=_Ay[1],_AB=function(_AC){var _AD=E(_Aw)[5],_AE=function(_AF){var _AG=new T(function(){return B(_zH(_Ak,_Ay));});return E(_Av)[1]<=9?function(_AH){return new F(function(){return _5B(_AD,[1,_Aj,new T(function(){return B(A(_AG,[_AH]));})]);});}:function(_AI){return [1,_5L,new T(function(){return B(_5B(_AD,[1,_Aj,new T(function(){return B(A(_AG,[[1,_5K,_AI]]));})]));})];};},_AJ=E(_AD);if(!_AJ[0]){return new F(function(){return _AE(_);});}else{if(E(E(_AJ[1])[1])==40){var _AK=E(_AJ[2]);if(!_AK[0]){return new F(function(){return _AE(_);});}else{if(E(E(_AK[1])[1])==44){return function(_AL){return [1,_At,new T(function(){return B(A(new T(function(){return B(_zH(_As,_Ay));}),[[1,_Aq,_AL]]));})];};}else{return new F(function(){return _AE(_);});}}}else{return new F(function(){return _AE(_);});}}},_AM=E(_Ay[2]);if(!_AM[0]){var _AN=E(_Aw),_AO=E(_Aa),_AP=hs_eqWord64(_AN[1],_AO[1]),_AQ=_AP;if(!E(_AQ)){return new F(function(){return _AB(_);});}else{var _AR=hs_eqWord64(_AN[2],_AO[2]),_AS=_AR;if(!E(_AS)){return new F(function(){return _AB(_);});}else{return function(_AT){return [1,_Ap,new T(function(){return B(A(new T(function(){var _AU=E(_AA);return B(_zO(_Au,_AU[3],_AU[4]));}),[[1,_Ao,_AT]]));})];};}}}else{if(!E(_AM[2])[0]){var _AV=E(_Aw),_AW=E(_zY),_AX=hs_eqWord64(_AV[1],_AW[1]),_AY=_AX;if(!E(_AY)){return new F(function(){return _AB(_);});}else{var _AZ=hs_eqWord64(_AV[2],_AW[2]),_B0=_AZ;if(!E(_B0)){return new F(function(){return _AB(_);});}else{var _B1=new T(function(){var _B2=E(_AM[1]);return B(_zO(_Ai,_B2[3],_B2[4]));}),_B3=new T(function(){var _B4=E(_AA);return B(_zO(_An,_B4[3],_B4[4]));});return E(_Av)[1]<=8?function(_B5){return new F(function(){return A(_B3,[new T(function(){return B(_5B(_Am,new T(function(){return B(A(_B1,[_B5]));},1)));})]);});}:function(_B6){return [1,_5L,new T(function(){return B(A(_B3,[new T(function(){return B(_5B(_Am,new T(function(){return B(A(_B1,[[1,_5K,_B6]]));},1)));})]));})];};}}}else{return new F(function(){return _AB(_);});}}}},_B7=function(_B8,_B9){return new F(function(){return A(_B8,[function(_){return new F(function(){return jsFind(toJSStr(E(_B9)));});}]);});},_Ba=[0],_Bb=function(_Bc){return E(E(_Bc)[3]);},_Bd=new T(function(){return [0,"value"];}),_Be=function(_Bf){return E(E(_Bf)[6]);},_Bg=function(_Bh){return E(E(_Bh)[1]);},_Bi=new T(function(){return B(unCStr("Char"));}),_Bj=new T(function(){var _Bk=hs_wordToWord64(3763641161),_Bl=_Bk,_Bm=hs_wordToWord64(1343745632),_Bn=_Bm;return [0,_Bl,_Bn,[0,_Bl,_Bn,_iN,_yI,_Bi],_1g];}),_Bo=function(_Bp){return E(_Bj);},_Bq=function(_Br){return E(_A0);},_Bs=new T(function(){return B(_jR(_Bq,_Bo));}),_Bt=new T(function(){return B(A(_Bs,[_]));}),_Bu=function(_Bv,_Bw,_Bx,_By,_Bz,_BA,_BB,_BC,_BD){var _BE=new T(function(){return B(A(_By,[_Ba]));});return new F(function(){return A(_Bw,[new T(function(){return B(_B7(E(_Bv)[2],_BD));}),function(_BF){var _BG=E(_BF);return _BG[0]==0?E(_BE):B(A(_Bw,[new T(function(){return B(A(E(_Bv)[2],[function(_){var _BH=jsGet(E(_BG[1])[1],E(_Bd)[1]),_BI=_BH;return [1,new T(function(){return fromJSStr(_BI);})];}]));}),function(_BJ){var _BK=E(_BJ);if(!_BK[0]){return E(_BE);}else{var _BL=_BK[1];if(!E(new T(function(){var _BM=B(A(_BA,[_])),_BN=E(_Bt),_BO=hs_eqWord64(_BM[1],_BN[1]),_BP=_BO;if(!E(_BP)){var _BQ=false;}else{var _BR=hs_eqWord64(_BM[2],_BN[2]),_BS=_BR,_BQ=E(_BS)==0?false:true;}var _BT=_BQ,_BU=_BT;return _BU;}))){var _BV=function(_BW){return new F(function(){return A(_By,[[1,_BL,new T(function(){return B(A(new T(function(){return B(_Be(_BC));}),[new T(function(){return B(A(new T(function(){return B(_Bb(_BC));}),[new T(function(){return B(unAppCStr("can\'t read \"",new T(function(){return B(_5B(_BL,new T(function(){return B(unAppCStr("\" as type ",new T(function(){var _BX=B(A(_BA,[_]));return B(A(_zO,[_Au,_BX[3],_BX[4],_1g]));})));})));})));})]));})]));})]]);});},_BY=B(A(new T(function(){return B(A(_Bg,[_BB,_60]));}),[_BL]));if(!_BY[0]){return new F(function(){return _BV(_);});}else{var _BZ=E(_BY[1]);return E(_BZ[2])[0]==0?E(_BY[2])[0]==0?B(A(_By,[[2,_BZ[1]]])):B(_BV(_)):B(_BV(_));}}else{return new F(function(){return A(_By,[[2,_BL]]);});}}}]));}]);});},_C0=1,_C1=function(_C2){return E(E(_C2)[9]);},_C3=function(_C4){return E(E(_C4)[2]);},_C5=function(_C6){return E(E(_C6)[2]);},_C7=function(_C8,_C9,_Ca){var _Cb=E(_Ca);if(!_Cb[0]){return [0];}else{var _Cc=_Cb[1],_Cd=B(A(_C8,[_])),_Ce=E(_Bt),_Cf=hs_eqWord64(_Cd[1],_Ce[1]),_Cg=_Cf;if(!E(_Cg)){return new F(function(){return A(_C5,[_C9,_Cc]);});}else{var _Ch=hs_eqWord64(_Cd[2],_Ce[2]),_Ci=_Ch;return E(_Ci)==0?B(A(_C5,[_C9,_Cc])):E(_Cc);}}},_Cj=function(_Ck,_Cl,_Cm,_Cn,_Co){var _Cp=new T(function(){return B(_C1(_Ck));}),_Cq=new T(function(){return B(_yR(_Cl));}),_Cr=new T(function(){return B(_yX(_Cq));}),_Cs=new T(function(){return B(_yX(_Cq));}),_Ct=new T(function(){return B(_yX(_Cq));}),_Cu=new T(function(){return B(_yX(_Cq));});return function(_Cv,_Cw,_Cx){return function(_Cy){return new F(function(){return A(new T(function(){return B(_yT(_Cq));}),[new T(function(){var _Cz=E(_Cv);return _Cz[0]==0?B(A(new T(function(){return B(_yT(_Cq));}),[new T(function(){return B(A(_Cs,[[0,_Cy,_Cy]]));}),function(_CA){var _CB=new T(function(){return E(E(_CA)[1]);}),_CC=new T(function(){return E(E(_CB)[2]);});return new F(function(){return A(new T(function(){return B(_yT(_Cq));}),[new T(function(){return B(A(_Ct,[[0,_c,new T(function(){var _CD=E(_CB);return [0,_CD[1],new T(function(){return [0,E(_CC)[1]+1|0];}),_CD[3],_CD[4],_CD[5],_CD[6],_CD[7]];})]]));}),function(_CE){return new F(function(){return A(_Cr,[[0,[1,_5S,new T(function(){return B(_5B(B(_5M(0,E(_CC)[1],_1g)),new T(function(){return E(E(_CB)[1]);},1)));})],new T(function(){return E(E(_CE)[2]);})]]);});}]);});}])):B(A(_Cr,[[0,_Cz[1],_Cy]]));}),function(_CF){var _CG=new T(function(){return E(E(_CF)[1]);});return new F(function(){return A(new T(function(){return B(_yT(_Cq));}),[new T(function(){var _CH=new T(function(){return E(E(_CF)[2]);});return B(A(_Cs,[[0,_CH,_CH]]));}),function(_CI){return new F(function(){return A(new T(function(){return B(_yT(_Cq));}),[new T(function(){return B(A(_Ct,[[0,_c,new T(function(){var _CJ=E(E(_CI)[1]);return [0,_CJ[1],_CJ[2],_C0,_CJ[4],_CJ[5],_CJ[6],_CJ[7]];})]]));}),function(_CK){return new F(function(){return A(new T(function(){return B(_yT(_Cq));}),[new T(function(){return B(A(new T(function(){return B(_Bu(new T(function(){return B(_z5(new T(function(){return B(_zw(_Cq));}),_Cl));}),function(_CL,_5m,_CM){return new F(function(){return _z9(_Cq,_CL,_5m,_CM);});},function(_CL,_5m,_CM){return new F(function(){return _zl(_Cq,_CL,_5m,_CM);});},function(_5m,_CM){return new F(function(){return _zA(_Cq,_5m,_CM);});},function(_CM){return new F(function(){return _zs(_Cq,_CM);});},_Cm,_Co,_Ck,_CG));}),[new T(function(){return E(E(_CK)[2]);})]));}),function(_CN){var _CO=E(_CN),_CP=_CO[2],_CQ=E(_CO[1]);switch(_CQ[0]){case 0:return new F(function(){return A(_Cu,[[0,[0,new T(function(){return B(A(_Cp,[_CG,_Cw,new T(function(){return B(_C7(_Cm,_Cn,_Cx));}),_61,_5A]));}),_5A],_CP]]);});break;case 1:return new F(function(){return A(_Cu,[[0,[0,new T(function(){return B(A(new T(function(){return B(_C3(new T(function(){return B(_zE(_Ck));})));}),[new T(function(){return B(A(_Cp,[_CG,_Cw,_CQ[1],_61,_5A]));}),_CQ[2]]));}),_5A],_CP]]);});break;default:var _CR=_CQ[1];return new F(function(){return A(_Cu,[[0,[0,new T(function(){return B(A(_Cp,[_CG,_Cw,new T(function(){return B(_C7(_Cm,_Cn,[1,_CR]));}),_61,_5A]));}),[1,_CR]],_CP]]);});}}]);});}]);});}]);});}]);});};};},_CS=new T(function(){return B(_Cj(_lY,_nw,_yP,_yH,_yu));}),_CT=function(_CU,_CV){return function(_CW,_){var _CX=B(A(_CU,[_CW,_])),_CY=_CX,_CZ=E(_CY),_D0=E(_CZ[1]);return [0,[0,function(_D1,_){var _D2=B(A(_D0[1],[_D1,_])),_D3=_D2,_D4=E(_D3),_D5=jsSetCB(_D4[1],E(new T(function(){return [0,toJSStr(B(_cS(_CV)))];}))[1],E(new T(function(){return B(_ep(_CV,function(_){var _D6=E(E(_CW)[4]),_D7=B(A(_D6[1],[_])),_D8=_D7,_D9=E(_D8);if(!_D9[0]){return _c;}else{var _Da=B(A(_D6[2],[_D9[1],_])),_Db=_Da;return _c;}}));}))),_Dc=_D5;return _D4;},_D0[2]],_CZ[2]];};},_Dd=function(_De,_Df,_Dg){return function(_Dh,_){var _Di=B(_fY(function(_Dj,_){var _Dk=B(A(new T(function(){return B(_CT(new T(function(){return B(_ig(new T(function(){return [0,E(_De)[1]+1|0];}),_iA));}),_cz));}),[_Dj,_])),_Dl=_Dk,_Dm=E(_Dl),_Dn=E(_Dm[1]),_Do=B(A(new T(function(){return B(_hK(_fD,function(_Dp,_){var _Dq=B(A(new T(function(){return B(A(_CS,[_5A,_iy,[1,_De]]));}),[_Dp,_])),_Dr=_Dq,_Ds=E(_Dr),_Dt=E(_Ds[1]);return [0,[0,function(_Du,_){var _Dv=B(A(_Dt[1],[_Du,_])),_Dw=_Dv,_Dx=B(A(_d,[_t,_Dw,_fE,_fG,_])),_Dy=_Dx;return _Dw;},_Dt[2]],_Ds[2]];},_cA));}),[_Dm[2],_])),_Dz=_Do,_DA=E(_Dz),_DB=E(_DA[1]),_DC=B(A(new T(function(){return B(_CT(new T(function(){return B(_ig(new T(function(){return [0,E(_De)[1]-1|0];}),_iC));}),_cz));}),[_DA[2],_])),_DD=_DC,_DE=E(_DD),_DF=E(_DE[1]);return [0,[0,function(_DG,_){var _DH=B(_c2(_Y,function(_bp,_){return new F(function(){return _be(_b1,_Df,_bp,_);});},_DG,_)),_DI=_DH,_DJ=B(A(_d,[_t,_DI,_b5,_cy,_])),_DK=_DJ,_DL=B(_c2(_Y,function(_DM,_){var _DN=B(A(_Dn[1],[_DM,_])),_DO=_DN,_DP=B(A(_DB[1],[_DM,_])),_DQ=_DP,_DR=B(A(_DF[1],[_DM,_])),_DS=_DR;return _DM;},_DG,_)),_DT=_DL,_DU=B(A(_d,[_t,_DT,_b5,_cy,_])),_DV=_DU;return _DG;},new T(function(){var _DW=E(_Dn[2]);if(!_DW[0]){var _DX=E(_DB[2]),_DY=_DX[0]==0?E(_DF[2]):E(_DX);}else{var _DY=E(_DW);}return _DY;})],_DE[2]];},function(_DZ){return function(_E0,_){return [0,new T(function(){if(E(_DZ)[1]<=0){var _E1=[1,function(_bp,_){return new F(function(){return _bu(_b1,new T(function(){return [0,toJSStr(E(_Dg))];}),_bp,_);});}];}else{var _E1=[0];}var _E2=_E1,_E3=_E2;return _E3;}),_E0];};},_Dh,_)),_E4=_Di,_E5=E(_E4),_E6=E(_E5[1]);return [0,[0,function(_bp,_){return new F(function(){return _cc(_E6[1],_bp,_);});},_E6[2]],_E5[2]];};},_E7=new T(function(){return B(unCStr("row vertical-align"));}),_E8=function(_E9){return E(E(_E9)[2]);},_Ea=function(_Eb){return E(E(_Eb)[5]);},_Ec=function(_Ed){return E(E(_Ed)[2]);},_Ee=function(_Ef){return E(E(_Ef)[3]);},_Eg=function(_Eh){return new F(function(){return _nI([1,function(_Ei){return new F(function(){return A(_vE,[_Ei,function(_Ej){return E(new T(function(){return B(_wU(function(_Ek){var _El=E(_Ek);return _El[0]==0?B(A(_Eh,[_El[1]])):[2];}));}));}]);});}],new T(function(){return [1,B(_xg(_Em,_Eh))];}));});},_Em=function(_En,_Eo){return new F(function(){return _Eg(_Eo);});},_Ep=function(_Eq){return new F(function(){return _nI(B(_nI([1,function(_Er){return new F(function(){return A(_vE,[_Er,function(_Es){return E(new T(function(){return B(_wU(function(_Et){var _Eu=E(_Et);return _Eu[0]==1?B(A(_Eq,[_Eu[1]])):[2];}));}));}]);});}],new T(function(){return B(_xZ(_Em,_Eq));}))),new T(function(){return [1,B(_xg(_Ev,_Eq))];}));});},_Ev=function(_Ew,_Ex){return new F(function(){return _Ep(_Ex);});},_Ey=new T(function(){return B(_Ep(_oH));}),_Ez=function(_xW){return new F(function(){return _ny(_Ey,_xW);});},_EA=new T(function(){return B(_Eg(_oH));}),_EB=function(_xW){return new F(function(){return _ny(_EA,_xW);});},_EC=function(_ED){return E(_EB);},_EE=[0,_EC,_Ez,_Em,_Ev],_EF=function(_EG){return E(E(_EG)[4]);},_EH=function(_EI,_EJ,_EK){return new F(function(){return _xZ(new T(function(){return B(_EF(_EI));}),_EK);});},_EL=function(_EM){return function(_b3){return new F(function(){return _ny(new T(function(){return B(_xZ(new T(function(){return B(_EF(_EM));}),_oH));}),_b3);});};},_EN=function(_EO,_EP){return function(_b3){return new F(function(){return _ny(new T(function(){return B(A(_EF,[_EO,_EP,_oH]));}),_b3);});};},_EQ=function(_ER){return [0,function(_xW){return new F(function(){return _EN(_ER,_xW);});},new T(function(){return B(_EL(_ER));}),new T(function(){return B(_EF(_ER));}),function(_xV,_xW){return new F(function(){return _EH(_ER,_xV,_xW);});}];},_ES=new T(function(){return B(_EQ(_EE));}),_ET=new T(function(){return B(unCStr("ACK"));}),_EU=new T(function(){return B(unCStr("BEL"));}),_EV=new T(function(){return B(unCStr("BS"));}),_EW=new T(function(){return B(unCStr("SP"));}),_EX=[1,_EW,_1g],_EY=new T(function(){return B(unCStr("US"));}),_EZ=[1,_EY,_EX],_F0=new T(function(){return B(unCStr("RS"));}),_F1=[1,_F0,_EZ],_F2=new T(function(){return B(unCStr("GS"));}),_F3=[1,_F2,_F1],_F4=new T(function(){return B(unCStr("FS"));}),_F5=[1,_F4,_F3],_F6=new T(function(){return B(unCStr("ESC"));}),_F7=[1,_F6,_F5],_F8=new T(function(){return B(unCStr("SUB"));}),_F9=[1,_F8,_F7],_Fa=new T(function(){return B(unCStr("EM"));}),_Fb=[1,_Fa,_F9],_Fc=new T(function(){return B(unCStr("CAN"));}),_Fd=[1,_Fc,_Fb],_Fe=new T(function(){return B(unCStr("ETB"));}),_Ff=[1,_Fe,_Fd],_Fg=new T(function(){return B(unCStr("SYN"));}),_Fh=[1,_Fg,_Ff],_Fi=new T(function(){return B(unCStr("NAK"));}),_Fj=[1,_Fi,_Fh],_Fk=new T(function(){return B(unCStr("DC4"));}),_Fl=[1,_Fk,_Fj],_Fm=new T(function(){return B(unCStr("DC3"));}),_Fn=[1,_Fm,_Fl],_Fo=new T(function(){return B(unCStr("DC2"));}),_Fp=[1,_Fo,_Fn],_Fq=new T(function(){return B(unCStr("DC1"));}),_Fr=[1,_Fq,_Fp],_Fs=new T(function(){return B(unCStr("DLE"));}),_Ft=[1,_Fs,_Fr],_Fu=new T(function(){return B(unCStr("SI"));}),_Fv=[1,_Fu,_Ft],_Fw=new T(function(){return B(unCStr("SO"));}),_Fx=[1,_Fw,_Fv],_Fy=new T(function(){return B(unCStr("CR"));}),_Fz=[1,_Fy,_Fx],_FA=new T(function(){return B(unCStr("FF"));}),_FB=[1,_FA,_Fz],_FC=new T(function(){return B(unCStr("VT"));}),_FD=[1,_FC,_FB],_FE=new T(function(){return B(unCStr("LF"));}),_FF=[1,_FE,_FD],_FG=new T(function(){return B(unCStr("HT"));}),_FH=[1,_FG,_FF],_FI=[1,_EV,_FH],_FJ=[1,_EU,_FI],_FK=[1,_ET,_FJ],_FL=new T(function(){return B(unCStr("ENQ"));}),_FM=[1,_FL,_FK],_FN=new T(function(){return B(unCStr("EOT"));}),_FO=[1,_FN,_FM],_FP=new T(function(){return B(unCStr("ETX"));}),_FQ=[1,_FP,_FO],_FR=new T(function(){return B(unCStr("STX"));}),_FS=[1,_FR,_FQ],_FT=new T(function(){return B(unCStr("SOH"));}),_FU=[1,_FT,_FS],_FV=new T(function(){return B(unCStr("NUL"));}),_FW=[1,_FV,_FU],_FX=[0,92],_FY=new T(function(){return B(unCStr("\\DEL"));}),_FZ=new T(function(){return B(unCStr("\\a"));}),_G0=new T(function(){return B(unCStr("\\\\"));}),_G1=new T(function(){return B(unCStr("\\SO"));}),_G2=new T(function(){return B(unCStr("\\r"));}),_G3=new T(function(){return B(unCStr("\\f"));}),_G4=new T(function(){return B(unCStr("\\v"));}),_G5=new T(function(){return B(unCStr("\\n"));}),_G6=new T(function(){return B(unCStr("\\t"));}),_G7=new T(function(){return B(unCStr("\\b"));}),_G8=function(_G9,_Ga){if(_G9<=127){var _Gb=E(_G9);switch(_Gb){case 92:return new F(function(){return _5B(_G0,_Ga);});break;case 127:return new F(function(){return _5B(_FY,_Ga);});break;default:if(_Gb<32){var _Gc=E(_Gb);switch(_Gc){case 7:return new F(function(){return _5B(_FZ,_Ga);});break;case 8:return new F(function(){return _5B(_G7,_Ga);});break;case 9:return new F(function(){return _5B(_G6,_Ga);});break;case 10:return new F(function(){return _5B(_G5,_Ga);});break;case 11:return new F(function(){return _5B(_G4,_Ga);});break;case 12:return new F(function(){return _5B(_G3,_Ga);});break;case 13:return new F(function(){return _5B(_G2,_Ga);});break;case 14:return new F(function(){return _5B(_G1,new T(function(){var _Gd=E(_Ga);if(!_Gd[0]){var _Ge=[0];}else{var _Ge=E(E(_Gd[1])[1])==72?B(unAppCStr("\\&",_Gd)):E(_Gd);}return _Ge;},1));});break;default:return new F(function(){return _5B([1,_FX,new T(function(){var _Gf=_Gc;return _Gf>=0?B(_9S(_FW,_Gf)):E(_9P);})],_Ga);});}}else{return [1,[0,_Gb],_Ga];}}}else{return [1,_FX,new T(function(){var _Gg=jsShowI(_G9),_Gh=_Gg;return B(_5B(fromJSStr(_Gh),new T(function(){var _Gi=E(_Ga);if(!_Gi[0]){var _Gj=[0];}else{var _Gk=E(_Gi[1])[1];if(_Gk<48){var _Gl=E(_Gi);}else{var _Gl=_Gk>57?E(_Gi):B(unAppCStr("\\&",_Gi));}var _Gm=_Gl,_Gn=_Gm,_Gj=_Gn;}return _Gj;},1)));})];}},_Go=[0,39],_Gp=[1,_Go,_1g],_Gq=new T(function(){return B(unCStr("\'\\\'\'"));}),_Gr=function(_Gs){var _Gt=E(E(_Gs)[1]);return _Gt==39?E(_Gq):[1,_Go,new T(function(){return B(_G8(_Gt,_Gp));})];},_Gu=[0,34],_Gv=new T(function(){return B(unCStr("\\\""));}),_Gw=function(_Gx,_Gy){var _Gz=E(_Gx);if(!_Gz[0]){return E(_Gy);}else{var _GA=_Gz[2],_GB=E(E(_Gz[1])[1]);if(_GB==34){return new F(function(){return _5B(_Gv,new T(function(){return B(_Gw(_GA,_Gy));},1));});}else{return new F(function(){return _G8(_GB,new T(function(){return B(_Gw(_GA,_Gy));}));});}}},_GC=function(_GD,_GE){return [1,_Gu,new T(function(){return B(_Gw(_GD,[1,_Gu,_GE]));})];},_GF=function(_GG){return new F(function(){return _5B(_Gq,_GG);});},_GH=function(_GI,_GJ){var _GK=E(E(_GJ)[1]);return _GK==39?E(_GF):function(_GL){return [1,_Go,new T(function(){return B(_G8(_GK,[1,_Go,_GL]));})];};},_GM=[0,_GH,_Gr,_GC],_GN=function(_GO){return E(E(_GO)[3]);},_GP=function(_GQ,_GR){return new F(function(){return A(_GN,[_GQ,_GR,_1g]);});},_GS=function(_GT,_GU,_GV){return new F(function(){return _dr(new T(function(){return B(_GN(_GT));}),_GU,_GV);});},_GW=function(_GX){return [0,function(_GY){return E(new T(function(){return B(_GN(_GX));}));},function(_GG){return new F(function(){return _GP(_GX,_GG);});},function(_GZ,_GG){return new F(function(){return _GS(_GX,_GZ,_GG);});}];},_H0=new T(function(){return B(_GW(_GM));}),_H1=new T(function(){return B(_Cj(_lY,_nw,_Bs,_H0,_ES));}),_H2=new T(function(){return B(unCStr("\u041e\u0431\u043d\u043e\u0432\u0438\u0442\u044c"));}),_H3=[1,_H2],_H4=new T(function(){return B(unCStr("submit"));}),_H5=new T(function(){return B(A(_H1,[_5A,_H4,_H3]));}),_H6=new T(function(){return B(_CT(_H5,_cz));}),_H7=function(_H8,_H9,_Ha){var _Hb=function(_Hc,_Hd){return new F(function(){return _nI([1,function(_He){return new F(function(){return A(_vE,[_He,function(_Hf){return E(new T(function(){return B(_wU(function(_Hg){var _Hh=E(_Hg);if(_Hh[0]==4){var _Hi=E(_Hh[1]);if(!_Hi[0]){return new F(function(){return A(_H8,[_Hh,_Hc,_Hd]);});}else{return E(E(_Hi[1])[1])==45?E(_Hi[2])[0]==0?E([1,function(_Hj){return new F(function(){return A(_vE,[_Hj,function(_Hk){return E(new T(function(){return B(_wU(function(_Hl){return new F(function(){return A(_H8,[_Hl,_Hc,function(_Hm){return new F(function(){return A(_Hd,[new T(function(){return [0, -E(_Hm)[1]];})]);});}]);});}));}));}]);});}]):B(A(_H8,[_Hh,_Hc,_Hd])):B(A(_H8,[_Hh,_Hc,_Hd]));}}else{return new F(function(){return A(_H8,[_Hh,_Hc,_Hd]);});}}));}));}]);});}],new T(function(){return [1,B(_xg(_Hb,_Hd))];}));});};return new F(function(){return _Hb(_H9,_Ha);});},_Hn=new T(function(){return [0,1/0];}),_Ho=function(_Hp,_Hq){return new F(function(){return A(_Hq,[_Hn]);});},_Hr=new T(function(){return [0,0/0];}),_Hs=function(_Ht,_Hu){return new F(function(){return A(_Hu,[_Hr]);});},_Hv=new T(function(){return B(unCStr("NaN"));}),_Hw=new T(function(){return B(unCStr("Infinity"));}),_Hx=function(_Hy,_Hz){return [2];},_HA=[0,1024],_HB=[0,-1021],_HC=new T(function(){return [0,0/0];}),_HD=new T(function(){return [0,-1/0];}),_HE=new T(function(){return [0,1/0];}),_HF=[0,0],_HG=function(_HH,_HI){while(1){var _HJ=E(_HH);if(!_HJ[0]){_HH=[1,I_fromInt(_HJ[1])];continue;}else{var _HK=E(_HI);if(!_HK[0]){_HH=_HJ;_HI=[1,I_fromInt(_HK[1])];continue;}else{return new F(function(){return I_fromRat(_HJ[1],_HK[1]);});}}}},_HL=function(_HM,_HN){var _HO=E(_HM);if(!_HO[0]){var _HP=_HO[1],_HQ=E(_HN);return _HQ[0]==0?_HP==_HQ[1]:I_compareInt(_HQ[1],_HP)==0?true:false;}else{var _HR=_HO[1],_HS=E(_HN);return _HS[0]==0?I_compareInt(_HR,_HS[1])==0?true:false:I_compare(_HR,_HS[1])==0?true:false;}},_HT=function(_HU,_HV){var _HW=E(_HU);if(!_HW[0]){var _HX=_HW[1],_HY=E(_HV);return _HY[0]==0?_HX<_HY[1]:I_compareInt(_HY[1],_HX)>0;}else{var _HZ=_HW[1],_I0=E(_HV);return _I0[0]==0?I_compareInt(_HZ,_I0[1])<0:I_compare(_HZ,_I0[1])<0;}},_I1=function(_I2,_I3){return !B(_HL(_I3,_HF))?[0,B(_HG(_I2,_I3))]:!B(_HL(_I2,_HF))?!B(_HT(_I2,_HF))?E(_HE):E(_HD):E(_HC);},_I4=function(_I5,_I6){while(1){var _I7=E(_I5);if(!_I7[0]){return E(_I6);}else{_I5=_I7[2];var _I8=_I6+1|0;_I6=_I8;continue;}}},_I9=new T(function(){return B(unCStr("ArithException"));}),_Ia=new T(function(){return B(unCStr("GHC.Exception"));}),_Ib=new T(function(){return B(unCStr("base"));}),_Ic=new T(function(){var _Id=hs_wordToWord64(4194982440),_Ie=_Id,_If=hs_wordToWord64(3110813675),_Ig=_If;return [0,_Ie,_Ig,[0,_Ie,_Ig,_Ib,_Ia,_I9],_1g];}),_Ih=function(_Ii){return E(_Ic);},_Ij=function(_Ik){var _Il=E(_Ik);return new F(function(){return _d6(B(_d4(_Il[1])),_Ih,_Il[2]);});},_Im=new T(function(){return B(unCStr("arithmetic underflow"));}),_In=new T(function(){return B(unCStr("arithmetic overflow"));}),_Io=new T(function(){return B(unCStr("Ratio has zero denominator"));}),_Ip=new T(function(){return B(unCStr("denormal"));}),_Iq=new T(function(){return B(unCStr("divide by zero"));}),_Ir=new T(function(){return B(unCStr("loss of precision"));}),_Is=function(_It){switch(E(_It)){case 0:return E(_In);case 1:return E(_Im);case 2:return E(_Ir);case 3:return E(_Iq);case 4:return E(_Ip);default:return E(_Io);}},_Iu=function(_Iv){return new F(function(){return _5B(_Im,_Iv);});},_Iw=function(_Iv){return new F(function(){return _5B(_In,_Iv);});},_Ix=function(_Iv){return new F(function(){return _5B(_Io,_Iv);});},_Iy=function(_Iv){return new F(function(){return _5B(_Ip,_Iv);});},_Iz=function(_Iv){return new F(function(){return _5B(_Iq,_Iv);});},_IA=function(_Iv){return new F(function(){return _5B(_Ir,_Iv);});},_IB=function(_IC){switch(E(_IC)){case 0:return E(_Iw);case 1:return E(_Iu);case 2:return E(_IA);case 3:return E(_Iz);case 4:return E(_Iy);default:return E(_Ix);}},_ID=function(_IE,_IF){return new F(function(){return _dr(_IB,_IE,_IF);});},_IG=function(_IH,_II){switch(E(_II)){case 0:return E(_Iw);case 1:return E(_Iu);case 2:return E(_IA);case 3:return E(_Iz);case 4:return E(_Iy);default:return E(_Ix);}},_IJ=[0,_IG,_Is,_ID],_IK=new T(function(){return [0,_Ih,_IJ,_IL,_Ij];}),_IL=function(_Iv){return [0,_IK,_Iv];},_IM=3,_IN=new T(function(){return B(_IL(_IM));}),_IO=new T(function(){return die(_IN);}),_IP=[0,0],_IQ=function(_IR,_IS){while(1){var _IT=E(_IR);if(!_IT[0]){var _IU=E(_IT[1]);if(_IU==(-2147483648)){_IR=[1,I_fromInt(-2147483648)];continue;}else{var _IV=E(_IS);if(!_IV[0]){return [0,_IU%_IV[1]];}else{_IR=[1,I_fromInt(_IU)];_IS=_IV;continue;}}}else{var _IW=_IT[1],_IX=E(_IS);return _IX[0]==0?[0,I_toInt(I_rem(_IW,I_fromInt(_IX[1])))]:[1,I_rem(_IW,_IX[1])];}}},_IY=function(_IZ,_J0){return !B(_HL(_J0,_IP))?B(_IQ(_IZ,_J0)):E(_IO);},_J1=function(_J2,_J3){while(1){if(!B(_HL(_J3,_IP))){var _J4=_J3,_J5=B(_IY(_J2,_J3));_J2=_J4;_J3=_J5;continue;}else{return E(_J2);}}},_J6=function(_J7){var _J8=E(_J7);if(!_J8[0]){var _J9=E(_J8[1]);return _J9==(-2147483648)?E(_q2):_J9<0?[0, -_J9]:E(_J8);}else{var _Ja=_J8[1];return I_compareInt(_Ja,0)>=0?E(_J8):[1,I_negate(_Ja)];}},_Jb=function(_Jc,_Jd){while(1){var _Je=E(_Jc);if(!_Je[0]){var _Jf=E(_Je[1]);if(_Jf==(-2147483648)){_Jc=[1,I_fromInt(-2147483648)];continue;}else{var _Jg=E(_Jd);if(!_Jg[0]){return [0,quot(_Jf,_Jg[1])];}else{_Jc=[1,I_fromInt(_Jf)];_Jd=_Jg;continue;}}}else{var _Jh=_Je[1],_Ji=E(_Jd);return _Ji[0]==0?[0,I_toInt(I_quot(_Jh,I_fromInt(_Ji[1])))]:[1,I_quot(_Jh,_Ji[1])];}}},_Jj=5,_Jk=new T(function(){return B(_IL(_Jj));}),_Jl=new T(function(){return die(_Jk);}),_Jm=function(_Jn,_Jo){if(!B(_HL(_Jo,_IP))){var _Jp=B(_J1(B(_J6(_Jn)),B(_J6(_Jo))));return !B(_HL(_Jp,_IP))?[0,B(_Jb(_Jn,_Jp)),B(_Jb(_Jo,_Jp))]:E(_IO);}else{return E(_Jl);}},_Jq=[0,1],_Jr=new T(function(){return B(unCStr("Negative exponent"));}),_Js=new T(function(){return B(err(_Jr));}),_Jt=[0,2],_Ju=new T(function(){return B(_HL(_Jt,_IP));}),_Jv=function(_Jw,_Jx){while(1){var _Jy=E(_Jw);if(!_Jy[0]){var _Jz=_Jy[1],_JA=E(_Jx);if(!_JA[0]){var _JB=_JA[1],_JC=subC(_Jz,_JB);if(!E(_JC[2])){return [0,_JC[1]];}else{_Jw=[1,I_fromInt(_Jz)];_Jx=[1,I_fromInt(_JB)];continue;}}else{_Jw=[1,I_fromInt(_Jz)];_Jx=_JA;continue;}}else{var _JD=E(_Jx);if(!_JD[0]){_Jw=_Jy;_Jx=[1,I_fromInt(_JD[1])];continue;}else{return [1,I_sub(_Jy[1],_JD[1])];}}}},_JE=function(_JF,_JG,_JH){while(1){if(!E(_Ju)){if(!B(_HL(B(_IQ(_JG,_Jt)),_IP))){if(!B(_HL(_JG,_Jq))){var _JI=B(_qb(_JF,_JF)),_JJ=B(_Jb(B(_Jv(_JG,_Jq)),_Jt)),_JK=B(_qb(_JF,_JH));_JF=_JI;_JG=_JJ;_JH=_JK;continue;}else{return new F(function(){return _qb(_JF,_JH);});}}else{var _JI=B(_qb(_JF,_JF)),_JJ=B(_Jb(_JG,_Jt));_JF=_JI;_JG=_JJ;continue;}}else{return E(_IO);}}},_JL=function(_JM,_JN){while(1){if(!E(_Ju)){if(!B(_HL(B(_IQ(_JN,_Jt)),_IP))){if(!B(_HL(_JN,_Jq))){return new F(function(){return _JE(B(_qb(_JM,_JM)),B(_Jb(B(_Jv(_JN,_Jq)),_Jt)),_JM);});}else{return E(_JM);}}else{var _JO=B(_qb(_JM,_JM)),_JP=B(_Jb(_JN,_Jt));_JM=_JO;_JN=_JP;continue;}}else{return E(_IO);}}},_JQ=function(_JR,_JS){return !B(_HT(_JS,_IP))?!B(_HL(_JS,_IP))?B(_JL(_JR,_JS)):E(_Jq):E(_Js);},_JT=[0,1],_JU=[0,0],_JV=[0,-1],_JW=function(_JX){var _JY=E(_JX);if(!_JY[0]){var _JZ=_JY[1];return _JZ>=0?E(_JZ)==0?E(_JU):E(_pR):E(_JV);}else{var _K0=I_compareInt(_JY[1],0);return _K0<=0?E(_K0)==0?E(_JU):E(_JV):E(_pR);}},_K1=function(_K2,_K3,_K4){while(1){var _K5=E(_K4);if(!_K5[0]){if(!B(_HT(_K2,_q8))){return [0,B(_qb(_K3,B(_JQ(_q7,_K2)))),_Jq];}else{var _K6=B(_JQ(_q7,B(_q3(_K2))));return new F(function(){return _Jm(B(_qb(_K3,B(_JW(_K6)))),B(_J6(_K6)));});}}else{var _K7=B(_Jv(_K2,_JT)),_K8=B(_pT(B(_qb(_K3,_q7)),B(_q9(E(_K5[1])[1]))));_K4=_K5[2];_K2=_K7;_K3=_K8;continue;}}},_K9=function(_Ka,_Kb){var _Kc=E(_Ka);if(!_Kc[0]){var _Kd=_Kc[1],_Ke=E(_Kb);return _Ke[0]==0?_Kd>=_Ke[1]:I_compareInt(_Ke[1],_Kd)<=0;}else{var _Kf=_Kc[1],_Kg=E(_Kb);return _Kg[0]==0?I_compareInt(_Kf,_Kg[1])>=0:I_compare(_Kf,_Kg[1])>=0;}},_Kh=function(_Ki){var _Kj=E(_Ki);if(!_Kj[0]){return new F(function(){return _Jm(B(_qb(B(_qj(new T(function(){return B(_q9(E(_Kj[1])[1]));}),_q8,_Kj[2])),_JT)),_JT);});}else{var _Kk=_Kj[1],_Kl=_Kj[3],_Km=E(_Kj[2]);if(!_Km[0]){var _Kn=E(_Kl);if(!_Kn[0]){return new F(function(){return _Jm(B(_qb(B(_qj(_q7,_q8,_Kk)),_JT)),_JT);});}else{var _Ko=_Kn[1];if(!B(_K9(_Ko,_q8))){var _Kp=B(_JQ(_q7,B(_q3(_Ko))));return new F(function(){return _Jm(B(_qb(B(_qj(_q7,_q8,_Kk)),B(_JW(_Kp)))),B(_J6(_Kp)));});}else{return new F(function(){return _Jm(B(_qb(B(_qb(B(_qj(_q7,_q8,_Kk)),B(_JQ(_q7,_Ko)))),_JT)),_JT);});}}}else{var _Kq=_Km[1],_Kr=E(_Kl);if(!_Kr[0]){return new F(function(){return _K1(_q8,B(_qj(_q7,_q8,_Kk)),_Kq);});}else{return new F(function(){return _K1(_Kr[1],B(_qj(_q7,_q8,_Kk)),_Kq);});}}}},_Ks=function(_Kt,_Ku){while(1){var _Kv=E(_Ku);if(!_Kv[0]){return [0];}else{if(!B(A(_Kt,[_Kv[1]]))){return E(_Kv);}else{_Ku=_Kv[2];continue;}}}},_Kw=function(_Kx,_Ky){var _Kz=E(_Kx);if(!_Kz[0]){var _KA=_Kz[1],_KB=E(_Ky);return _KB[0]==0?_KA>_KB[1]:I_compareInt(_KB[1],_KA)<0;}else{var _KC=_Kz[1],_KD=E(_Ky);return _KD[0]==0?I_compareInt(_KC,_KD[1])>0:I_compare(_KC,_KD[1])>0;}},_KE=[0,0],_KF=function(_KG,_KH){return E(_KG)[1]==E(_KH)[1];},_KI=function(_KJ){return new F(function(){return _KF(_KE,_KJ);});},_KK=[0,E(_q8),E(_Jq)],_KL=[1,_KK],_KM=[0,-2147483648],_KN=[0,2147483647],_KO=function(_KP,_KQ,_KR){var _KS=E(_KR);if(!_KS[0]){return [1,new T(function(){var _KT=B(_Kh(_KS));return [0,E(_KT[1]),E(_KT[2])];})];}else{var _KU=E(_KS[3]);if(!_KU[0]){return [1,new T(function(){var _KV=B(_Kh(_KS));return [0,E(_KV[1]),E(_KV[2])];})];}else{var _KW=_KU[1];if(!B(_Kw(_KW,_KN))){if(!B(_HT(_KW,_KM))){var _KX=function(_KY){var _KZ=_KY+B(_ro(_KW))|0;return _KZ<=(E(_KQ)[1]+3|0)?_KZ>=(E(_KP)[1]-3|0)?[1,new T(function(){var _L0=B(_Kh(_KS));return [0,E(_L0[1]),E(_L0[2])];})]:E(_KL):[0];},_L1=B(_Ks(_KI,_KS[1]));if(!_L1[0]){var _L2=E(_KS[2]);if(!_L2[0]){return E(_KL);}else{var _L3=B(_dO(_KI,_L2[1]));if(!E(_L3[2])[0]){return E(_KL);}else{return new F(function(){return _KX( -B(_I4(_L3[1],0)));});}}}else{return new F(function(){return _KX(B(_I4(_L1,0)));});}}else{return [0];}}else{return [0];}}}},_L4=function(_L5){var _L6=E(_L5);switch(_L6[0]){case 3:var _L7=_L6[1];return !B(_h5(_L7,_Hw))?!B(_h5(_L7,_Hv))?E(_Hx):E(_Hs):E(_Ho);case 5:var _L8=B(_KO(_HB,_HA,_L6[1]));return _L8[0]==0?E(_Ho):function(_L9,_La){return new F(function(){return A(_La,[new T(function(){var _Lb=E(_L8[1]);return B(_I1(_Lb[1],_Lb[2]));})]);});};default:return E(_Hx);}},_Lc=function(_xV,_xW){return new F(function(){return _H7(_L4,_xV,_xW);});},_Ld=function(_Le,_Lf){return new F(function(){return _xZ(_Lc,_Lf);});},_Lg=function(_Lh){return function(_b3){return new F(function(){return _ny(new T(function(){return B(_H7(_L4,_Lh,_oH));}),_b3);});};},_Li=new T(function(){return B(_xZ(_Lc,_oH));}),_Lj=function(_xW){return new F(function(){return _ny(_Li,_xW);});},_Lk=[0,_Lg,_Lj,_Lc,_Ld],_Ll=function(_Lm){var _Ln=jsShow(E(_Lm)[1]),_Lo=_Ln;return new F(function(){return fromJSStr(_Lo);});},_Lp=function(_Lq){return function(_b3){return new F(function(){return _5B(new T(function(){return B(_Ll(_Lq));}),_b3);});};},_Lr=[0,45],_Ls=function(_Lt,_Lu,_Lv){var _Lw=function(_Lx){var _Ly=new T(function(){return B(A(_Lt,[[0, -_Lv]]));});return E(_Lu)[1]<=6?function(_Lz){return [1,_Lr,new T(function(){return B(A(_Ly,[_Lz]));})];}:function(_LA){return [1,_5L,[1,_Lr,new T(function(){return B(A(_Ly,[[1,_5K,_LA]]));})]];};};if(_Lv>=0){var _LB=isDoubleNegativeZero(_Lv),_LC=_LB;return E(_LC)==0?B(A(_Lt,[[0,_Lv]])):B(_Lw(_));}else{return new F(function(){return _Lw(_);});}},_LD=function(_LE){return new F(function(){return A(_Ls,[_Lp,_Au,E(_LE)[1],_1g]);});},_LF=[0,0],_LG=function(_LH){return new F(function(){return _Ls(_Lp,_LF,E(_LH)[1]);});},_LI=function(_LJ,_LK){return new F(function(){return _dr(_LG,_LJ,_LK);});},_LL=function(_LM,_LN){return new F(function(){return _Ls(_Lp,_LM,E(_LN)[1]);});},_LO=[0,_LL,_LD,_LI],_LP=new T(function(){return B(unCStr("Double"));}),_LQ=new T(function(){var _LR=hs_wordToWord64(2568654869),_LS=_LR,_LT=hs_wordToWord64(3333976055),_LU=_LT;return [0,_LS,_LU,[0,_LS,_LU,_iN,_yI,_LP],_1g];}),_LV=function(_LW){return E(_LQ);},_LX=new T(function(){return B(_Cj(_lY,_nw,_LV,_LO,_Lk));}),_LY=new T(function(){return [0,"\u0414\u043b\u0438\u043d\u0430 \u0438\u043d\u0434\u0438\u0432\u0438\u0434\u0430: "];}),_LZ=new T(function(){return B(unCStr("\u0434\u043b\u0438\u043d\u0430 \u0438\u043d\u0434\u0438\u0432\u0438\u0434\u0430 \u0434\u043e\u043b\u0436\u043d\u0430 \u0431\u044b\u0442\u044c \u043f\u043e\u043b\u043e\u0436\u0438\u0442\u0435\u043b\u044c\u043d\u0430"));}),_M0=new T(function(){return [0,"\u0427\u0438\u0441\u043b\u043e \u0433\u043e\u0440\u043e\u0434\u043e\u0432: "];}),_M1=new T(function(){return B(unCStr("\u0447\u0438\u0441\u043b\u043e \u0433\u043e\u0440\u043e\u0434\u043e\u0432 \u0434\u043e\u043b\u0436\u043d\u043e \u0431\u044b\u0442\u044c \u043f\u043e\u043b\u043e\u0436\u0438\u0442\u0435\u043b\u044c\u043d\u043e"));}),_M2=[2,_],_M3=function(_M4,_M5){while(1){var _M6=E(_M4);if(!_M6){return E(_M5);}else{var _M7=E(_M5);if(!_M7[0]){return [0];}else{_M4=_M6-1|0;_M5=_M7[2];continue;}}}},_M8=function(_M9,_Ma,_Mb,_Mc){while(1){var _Md=(function(_Me,_Mf,_Mg,_Mh){var _Mi=B(_I4(_Me,0));if(_Mf>=_Mi){var _Mj=B(_5B(_Me,new T(function(){var _Mk=(_Mf+1|0)-_Mi|0;if(_Mk>0){var _Ml=function(_Mm){return _Mm>1?[1,_Mg,new T(function(){return B(_Ml(_Mm-1|0));})]:E([1,_Mg,_1g]);},_Mn=B(_Ml(_Mk));}else{var _Mn=[0];}var _Mo=_Mn,_Mp=_Mo,_Mq=_Mp;return _Mq;},1))),_Mr=_Mf,_Ms=_Mg,_Mt=_Mh;_M9=_Mj;_Ma=_Mr;_Mb=_Ms;_Mc=_Mt;return null;}else{var _Mu=[1,_Mh,new T(function(){var _Mv=_Mf+1|0;return _Mv>=0?B(_M3(_Mv,_Me)):E(_Me);})];if(_Mf>0){var _Mw=function(_Mx,_My){var _Mz=E(_Mx);if(!_Mz[0]){return E(_Mu);}else{var _MA=_Mz[1];return _My>1?[1,_MA,new T(function(){return B(_Mw(_Mz[2],_My-1|0));})]:[1,_MA,_Mu];}};return new F(function(){return _Mw(_Me,_Mf);});}else{return E(_Mu);}}})(_M9,_Ma,_Mb,_Mc);if(_Md!=null){return _Md;}}},_MB=new T(function(){return B(unCStr("class"));}),_MC=new T(function(){return B(unCStr("row"));}),_MD=function(_ME,_MF){if(_ME<=_MF){var _MG=function(_MH){return [1,[0,_MH],new T(function(){if(_MH!=_MF){var _MI=B(_MG(_MH+1|0));}else{var _MI=[0];}var _MJ=_MI;return _MJ;})];};return new F(function(){return _MG(_ME);});}else{return [0];}},_MK=function(_ML,_MM){while(1){var _MN=(function(_MO,_MP){var _MQ=E(_MP);if(!_MQ[0]){return E(_MO);}else{_ML=function(_MR,_){var _MS=B(A(_MO,[_MR,_])),_MT=_MS,_MU=E(_MT),_MV=E(_MU[1]),_MW=B(A(_MQ[1],[_MU[2],_])),_MX=_MW,_MY=E(_MX),_MZ=E(_MY[1]);return [0,[0,function(_N0,_){var _N1=B(A(_MV[1],[_N0,_])),_N2=_N1,_N3=B(A(_MZ[1],[_N0,_])),_N4=_N3;return _N0;},new T(function(){var _N5=E(_MV[2]);return _N5[0]==0?E(_MZ[2]):E(_N5);})],_MY[2]];};_MM=_MQ[2];return null;}})(_ML,_MM);if(_MN!=null){return _MN;}}},_N6=function(_N7,_N8){while(1){var _N9=(function(_Na,_Nb){var _Nc=E(_Nb);if(!_Nc[0]){return E(_Na);}else{_N7=function(_Nd,_){var _Ne=B(A(_Na,[_Nd,_])),_Nf=_Ne,_Ng=E(_Nf),_Nh=E(_Ng[1]),_Ni=B(A(_Nc[1],[_Ng[2],_])),_Nj=_Ni,_Nk=E(_Nj),_Nl=E(_Nk[1]);return [0,[0,function(_Nm,_){var _Nn=B(A(_Nh[1],[_Nm,_])),_No=_Nn,_Np=B(A(_Nl[1],[_Nm,_])),_Nq=_Np;return _Nm;},new T(function(){var _Nr=E(_Nh[2]);return _Nr[0]==0?E(_Nl[2]):E(_Nr);})],_Nk[2]];};_N8=_Nc[2];return null;}})(_N7,_N8);if(_N9!=null){return _N9;}}},_Ns=[0,0],_Nt=new T(function(){return B(unCStr("size"));}),_Nu=[0,53],_Nv=[1,_Nu,_1g],_Nw=[0,_Nt,_Nv],_Nx=[1,_Nw,_1g],_Ny=new T(function(){return B(_Cj(_lY,_nw,_yP,_yH,_yu));}),_Nz=[0,_K,_5A],_NA=function(_NB,_){return [0,_Nz,_NB];},_NC=[1,_Ns,_1g],_ND=function(_NE){return _NE>1?[1,_Ns,new T(function(){return B(_ND(_NE-1|0));})]:E(_NC);},_NF=function(_NG){return function(_b3,_b4){return new F(function(){return _76(new T(function(){var _NH=E(_NG),_NI=_NH[1],_NJ=E(_NH[2])[1],_NK=_NJ-1|0;if(0<=_NK){var _NL=new T(function(){return [0,B(_I4(_NI,0))];}),_NM=function(_NN){return [1,function(_NO,_){var _NP=B(A(new T(function(){var _NQ=B(_8X(function(_NR){return function(_b3,_b4){return new F(function(){return _76(function(_NS,_){var _NT=B(A(new T(function(){return B(_hK(_fD,new T(function(){return B(A(_Ny,[_5A,_iy,[1,new T(function(){if(!E(new T(function(){return _NN<E(_NL)[1];}))){var _NU=E(_Ns);}else{var _NV=E(_NR)[1];if(_NV>=E(new T(function(){if(!E(E(_NL)[1])){var _NW=E(_Ns);}else{var _NW=[0,B(_I4(B(_9S(_NI,0)),0))];}var _NX=_NW;return _NX;}))[1]){var _NY=E(_Ns);}else{var _NY=_NV>=0?B(_9S(new T(function(){return _NN>=0?B(_9S(_NI,_NN)):E(_9P);}),_NV)):E(_9P);}var _NZ=_NY,_O0=_NZ,_O1=_O0,_NU=_O1;}return _NU;})]]));}),_M2));}),[_NS,_])),_O2=_NT,_O3=E(_O2),_O4=E(_O3[1]);return [0,[0,new T(function(){return B(_5f(_O4[1],_Nx));}),_O4[2]],_O3[2]];},function(_O5,_O6,_){return [0,[0,_K,[1,new T(function(){return B(_M8(_NI,_NN,new T(function(){return _NJ>0?B(_ND(_NJ)):[0];}),new T(function(){return B(_M8(new T(function(){if(_NN>=E(_NL)[1]){var _O7=[0];}else{var _O7=_NN>=0?B(_9S(_NI,_NN)):E(_9P);}var _O8=_O7,_O9=_O8;return _O9;}),E(_NR)[1],_Ns,_O5));})));})]],_O6];},_b3,_b4);});};},new T(function(){return B(_MD(0,_NJ-1|0));})));return _NQ[0]==0?E(_NA):B(_MK(_NQ[1],_NQ[2]));}),[_NO,_])),_Oa=_NP,_Ob=E(_Oa),_Oc=E(_Ob[1]);return [0,[0,function(_Od,_){var _Oe=B(_c2(_Y,_Oc[1],_Od,_)),_Of=_Oe,_Og=B(A(_d,[_t,_Of,_MB,_MC,_])),_Oh=_Og;return _Of;},_Oc[2]],_Ob[2]];},new T(function(){if(_NN!=_NK){var _Oi=B(_NM(_NN+1|0));}else{var _Oi=[0];}var _Oj=_Oi;return _Oj;})];},_Ok=B(_NM(0)),_Ol=_Ok[0]==0?E(_NA):B(_N6(_Ok[1],_Ok[2]));}else{var _Ol=E(_NA);}var _Om=_Ol,_On=_Om,_Oo=_On,_Op=_Oo;return _Op;}),function(_Oq,_Or,_){return [0,[0,_K,[1,new T(function(){var _Os=E(_NG);return [0,_Oq,_Os[2],_Os[3],_Os[4]];})]],_Or];},_b3,_b4);});};},_Ot=function(_Ou){return E(E(_Ou)[3]);},_Ov=function(_Ow){return E(E(_Ow)[1]);},_Ox=function(_Oy){return E(E(_Oy)[4]);},_Oz=function(_OA){return E(E(_OA)[6]);},_OB=function(_OC){var _OD=new T(function(){return E(E(_OC)[4]);});return function(_OE,_){var _OF=B(_76(new T(function(){return B(_Dd(new T(function(){return B(_Ee(_OC));}),_LY,_LZ));}),function(_OG,_OH,_){return [0,[0,_K,[1,new T(function(){var _OI=E(_OC);return [0,_OI[1],_OI[2],_OG,_OI[4]];})]],_OH];},_OE,_)),_OJ=_OF,_OK=E(_OJ),_OL=E(_OK[1]),_OM=B(_76(new T(function(){return B(_Dd(new T(function(){return B(_Ec(_OC));}),_M0,_M1));}),function(_ON,_OO,_){return [0,[0,_K,[1,new T(function(){var _OP=E(_OC);return [0,_OP[1],_ON,_OP[3],_OP[4]];})]],_OO];},_OK[2],_)),_OQ=_OM,_OR=E(_OQ),_OS=E(_OR[1]),_OT=B(A(new T(function(){return B(_NF(_OC));}),[_OR[2],_])),_OU=_OT,_OV=E(_OU),_OW=E(_OV[1]),_OX=B(_76(function(_OY,_){var _OZ=B(_fY(new T(function(){return B(A(_LX,[_5A,_iy,[1,new T(function(){return B(_Ov(_OD));})]]));}),_bG,_OY,_)),_P0=_OZ,_P1=E(_P0),_P2=E(_P1[1]),_P3=B(_fY(new T(function(){return B(A(_LX,[_5A,_iy,[1,new T(function(){return B(_E8(_OD));})]]));}),_bS,_P1[2],_)),_P4=_P3,_P5=E(_P4),_P6=E(_P5[1]),_P7=B(_fY(new T(function(){return B(A(_CS,[_5A,_iy,[1,new T(function(){return B(_Ot(_OD));})]]));}),_cm,_P5[2],_)),_P8=_P7,_P9=E(_P8),_Pa=E(_P9[1]),_Pb=B(_fY(new T(function(){return B(A(_CS,[_5A,_iy,[1,new T(function(){return B(_Ox(_OD));})]]));}),_cm,_P9[2],_)),_Pc=_Pb,_Pd=E(_Pc),_Pe=E(_Pd[1]),_Pf=B(_fY(new T(function(){return B(A(_CS,[_5A,_iy,[1,new T(function(){return B(_Ea(_OD));})]]));}),_cm,_Pd[2],_)),_Pg=_Pf,_Ph=E(_Pg),_Pi=E(_Ph[1]),_Pj=B(_76(function(_Pk,_){var _Pl=B(A(new T(function(){return B(A(_LX,[_5A,_iy,new T(function(){return B(_Oz(_OD));})]));}),[_Pk,_])),_Pm=_Pl,_Pn=E(_Pm),_Po=E(_Pn[1]);return [0,[0,function(_bp,_){return new F(function(){return _cc(function(_Pp,_){var _Pq=B(_c2(_Y,_cx,_Pp,_)),_Pr=_Pq,_Ps=B(A(_d,[_t,_Pr,_b5,_cy,_])),_Pt=_Ps,_Pu=B(_c2(_Y,_Po[1],_Pp,_)),_Pv=_Pu,_Pw=B(A(_d,[_t,_Pv,_b5,_cy,_])),_Px=_Pw;return _Pp;},_bp,_);});},_Po[2]],_Pn[2]];},_ct,_Ph[2],_)),_Py=_Pj,_Pz=E(_Py),_PA=E(_Pz[1]),_PB=B(A(_H6,[_Pz[2],_])),_PC=_PB,_PD=E(_PC);return [0,[0,function(_bp,_){return new F(function(){return _cc(function(_PE,_){var _PF=B(_be(_b1,_b9,_PE,_)),_PG=_PF,_PH=B(A(_d,[_t,_PG,_ba,_bb,_])),_PI=_PH,_PJ=B(_c2(_Y,function(_PK,_){var _PL=B(_c2(_Y,_bo,_PK,_)),_PM=_PL,_PN=B(A(_d,[_t,_PM,_b5,_cy,_])),_PO=_PN,_PP=B(_c2(_Y,_P2[1],_PK,_)),_PQ=_PP,_PR=B(A(_d,[_t,_PQ,_b5,_cy,_])),_PS=_PR;return _PK;},_PE,_)),_PT=_PJ,_PU=B(A(_d,[_t,_PT,_b5,_bq,_])),_PV=_PU,_PW=B(_c2(_Y,function(_PX,_){var _PY=B(_c2(_Y,_bO,_PX,_)),_PZ=_PY,_Q0=B(A(_d,[_t,_PZ,_b5,_cy,_])),_Q1=_Q0,_Q2=B(_c2(_Y,_P6[1],_PX,_)),_Q3=_Q2,_Q4=B(A(_d,[_t,_Q3,_b5,_cy,_])),_Q5=_Q4;return _PX;},_PE,_)),_Q6=_PW,_Q7=B(A(_d,[_t,_Q6,_b5,_bq,_])),_Q8=_Q7,_Q9=B(_c2(_Y,function(_Qa,_){var _Qb=B(_c2(_Y,_c0,_Qa,_)),_Qc=_Qb,_Qd=B(A(_d,[_t,_Qc,_b5,_cy,_])),_Qe=_Qd,_Qf=B(_c2(_Y,_Pa[1],_Qa,_)),_Qg=_Qf,_Qh=B(A(_d,[_t,_Qg,_b5,_cy,_])),_Qi=_Qh;return _Qa;},_PE,_)),_Qj=_Q9,_Qk=B(A(_d,[_t,_Qj,_b5,_bq,_])),_Ql=_Qk,_Qm=B(_c2(_Y,function(_Qn,_){var _Qo=B(_c2(_Y,_cq,_Qn,_)),_Qp=_Qo,_Qq=B(A(_d,[_t,_Qp,_b5,_cy,_])),_Qr=_Qq,_Qs=B(_c2(_Y,_Pe[1],_Qn,_)),_Qt=_Qs,_Qu=B(A(_d,[_t,_Qt,_b5,_cy,_])),_Qv=_Qu;return _Qn;},_PE,_)),_Qw=_Qm,_Qx=B(A(_d,[_t,_Qw,_b5,_bq,_])),_Qy=_Qx,_Qz=B(_c2(_Y,function(_QA,_){var _QB=B(_c2(_Y,_cs,_QA,_)),_QC=_QB,_QD=B(A(_d,[_t,_QC,_b5,_cy,_])),_QE=_QD,_QF=B(_c2(_Y,_Pi[1],_QA,_)),_QG=_QF,_QH=B(A(_d,[_t,_QG,_b5,_cy,_])),_QI=_QH;return _QA;},_PE,_)),_QJ=_Qz,_QK=B(A(_d,[_t,_QJ,_b5,_bq,_])),_QL=_QK,_QM=B(A(_PA[1],[_PE,_])),_QN=_QM,_QO=B(A(E(_PD[1])[1],[_PE,_])),_QP=_QO;return _PE;},_bp,_);});},new T(function(){var _QQ=E(_P2[2]);if(!_QQ[0]){var _QR=[0];}else{var _QS=E(_P6[2]);if(!_QS[0]){var _QT=[0];}else{var _QU=E(_Pa[2]);if(!_QU[0]){var _QV=[0];}else{var _QW=E(_Pe[2]);if(!_QW[0]){var _QX=[0];}else{var _QY=E(_Pi[2]);if(!_QY[0]){var _QZ=[0];}else{var _R0=E(_PA[2]),_QZ=_R0[0]==0?[0]:[1,[0,_QQ[1],_QS[1],_QU[1],_QW[1],_QY[1],_R0[1]]];}var _QX=_QZ;}var _QV=_QX;}var _QT=_QV;}var _QR=_QT;}return _QR;})],_PD[2]];},function(_R1,_R2,_){return [0,[0,_K,[1,new T(function(){var _R3=E(_OC);return [0,_R3[1],_R3[2],_R3[3],_R1];})]],_R2];},_OV[2],_)),_R4=_OX,_R5=E(_R4),_R6=E(_R5[1]);return [0,[0,function(_R7,_){var _R8=B(_c2(_Y,function(_R9,_){var _Ra=B(_c2(_Y,_K,_R9,_)),_Rb=_Ra,_Rc=B(A(_d,[_t,_Rb,_b5,_b6,_])),_Rd=_Rc,_Re=B(_c2(_Y,function(_Rf,_){var _Rg=B(_c2(_Y,function(_Rh,_){var _Ri=B(_c2(_Y,_OL[1],_Rh,_)),_Rj=_Ri,_Rk=B(A(_d,[_t,_Rj,_b5,_bq,_])),_Rl=_Rk,_Rm=B(_c2(_Y,_OS[1],_Rh,_)),_Rn=_Rm,_Ro=B(A(_d,[_t,_Rn,_b5,_bq,_])),_Rp=_Ro,_Rq=B(A(_OW[1],[_Rh,_])),_Rr=_Rq,_Rs=B(A(_R6[1],[_Rh,_])),_Rt=_Rs;return _Rh;},_Rf,_)),_Ru=_Rg,_Rv=B(A(_d,[_t,_Ru,_b5,_b8,_])),_Rw=_Rv;return _Ru;},_R9,_)),_Rx=_Re,_Ry=B(A(_d,[_t,_Rx,_b5,_b7,_])),_Rz=_Ry;return _R9;},_R7,_)),_RA=_R8,_RB=B(A(_d,[_t,_RA,_b5,_E7,_])),_RC=_RB;return _RA;},new T(function(){var _RD=E(_OL[2]);if(!_RD[0]){var _RE=E(_OS[2]);if(!_RE[0]){var _RF=E(_OW[2]),_RG=_RF[0]==0?E(_R6[2]):E(_RF);}else{var _RG=E(_RE);}var _RH=_RG;}else{var _RH=E(_RD);}return _RH;})],_R5[2]];};},_RI=function(_RJ,_RK){var _RL=E(_RK)[1];return _RL>=0?B(_9S(_RJ,_RL)):E(_9P);},_RM=function(_RN,_RO){return E(_RN)[1]!=E(_RO)[1];},_RP=[0,_KF,_RM],_RQ=function(_RR,_RS){var _RT=function(_RU,_RV){while(1){var _RW=(function(_RX,_RY){var _RZ=E(_RX);if(!_RZ[0]){return [0];}else{var _S0=_RZ[1],_S1=_RZ[2];if(!B(_qS(_RR,_S0,_RY))){return [1,_S0,new T(function(){return B(_RT(_S1,[1,_S0,_RY]));})];}else{_RU=_S1;var _S2=_RY;_RV=_S2;return null;}}})(_RU,_RV);if(_RW!=null){return _RW;}}};return new F(function(){return _RT(_RS,_1g);});},_S3=function(_S4,_S5){var _S6=B(_a6(_a2,_S5)),_S7=B(_I4(_S6,0));if(!_S7){return -9001;}else{var _S8=B(_I4(_S5,0));return 100*_S8/_S7+B(_9X(B(_84(function(_S9,_Sa,_Sb){return [1,new T(function(){var _Sc=E(_Sa)[1];if(_Sc>=0){var _Sd=B(_9S(B(_RI(new T(function(){return E(E(_S4)[1]);}),_S9)),_Sc));}else{var _Sd=E(_9P);}var _Se=_Sd,_Sf=_Se;return _Sf;}),_Sb];},_1g,_S6,new T(function(){var _Sg=E(_S6);return _Sg[0]==0?E(_al):E(_Sg[2]);},1))),0))+(imul(-300,_S8-B(_I4(B(_RQ(_RP,_S5)),0))|0)|0);}},_Sh=function(_Si,_Sj){return [0,B(_S3(_Si,_Sj))];},_Sk=new T(function(){return B(unCStr("Chromosomes have different lengths"));}),_Sl=new T(function(){return B(err(_Sk));}),_Sm=function(_Sn){return E(_Sl);},_So=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_Sp=function(_Sq){return new F(function(){return _dL([0,new T(function(){return B(_e0(_Sq,_So));})],_dI);});},_Sr=function(_Ss){return new F(function(){return _Sp("Commi/Genetic.hs:38:11-22|[b0, b1]");});},_St=new T(function(){return B(_Sr(_));}),_Su=function(_Sv){return new F(function(){return _Sp("Commi/Genetic.hs:37:11-22|[a0, a1]");});},_Sw=new T(function(){return B(_Su(_));}),_Sx=function(_Sy,_Sz){return [0,E(_Sz)[1],_Sy];},_SA=function(_SB,_SC){var _SD=E(_SC);return [0,_SD[1],new T(function(){return B(A(_SB,[_SD[2]]));})];},_SE=[0,_SA,_Sx],_SF=function(_SG,_SH,_SI,_SJ){return new F(function(){return _Jm(B(_pT(B(_qb(_SG,_SJ)),B(_qb(_SI,_SH)))),B(_qb(_SH,_SJ)));});},_SK=function(_SL,_SM){var _SN=E(_SL),_SO=E(_SM),_SP=B(_SF(_SN[1],_SN[2],_SO[1],_SO[2]));return [0,E(_SP[1]),E(_SP[2])];},_SQ=function(_SR,_SS,_ST,_SU){return new F(function(){return _Jm(B(_Jv(B(_qb(_SR,_SU)),B(_qb(_ST,_SS)))),B(_qb(_SS,_SU)));});},_SV=function(_SW,_SX){var _SY=E(_SW),_SZ=E(_SX),_T0=B(_SQ(_SY[1],_SY[2],_SZ[1],_SZ[2]));return [0,E(_T0[1]),E(_T0[2])];},_T1=function(_T2,_T3){var _T4=E(_T2),_T5=E(_T3),_T6=B(_Jm(B(_qb(_T4[1],_T5[1])),B(_qb(_T4[2],_T5[2]))));return [0,E(_T6[1]),E(_T6[2])];},_T7=function(_T8){var _T9=E(_T8);return [0,E(B(_J6(_T9[1]))),E(_T9[2])];},_Ta=function(_Tb){return [0,E(E(_Tb)),E(_Jq)];},_Tc=function(_Td){var _Te=E(_Td);return [0,E(B(_q3(_Te[1]))),E(_Te[2])];},_Tf=function(_Tg){return [0,E(B(_JW(E(_Tg)[1]))),E(_Jq)];},_Th=[0,_SK,_T1,_SV,_Tc,_T7,_Tf,_Ta],_Ti=function(_Tj,_Tk){var _Tl=E(_Tj),_Tm=E(_Tk);return new F(function(){return _HT(B(_qb(_Tl[1],_Tm[2])),B(_qb(_Tm[1],_Tl[2])));});},_Tn=[0,2147483646],_To=[0,0],_Tp=[0,_To,_Tn],_Tq=function(_Tr){return E(_Tp);},_Ts=function(_Tt){var _Tu=jsTrunc(_Tt),_Tv=_Tu;return [0,_Tv];},_Tw=new T(function(){return B(_a("(function(s){return s[0];})"));}),_Tx=function(_Ty,_){var _Tz=B(A(_Tw,[E(_Ty),_])),_TA=_Tz;return new T(function(){return B(_Ts(_TA));});},_TB=function(_TC,_){return new F(function(){return _Tx(_TC,_);});},_TD=function(_TE,_TF){var _TG=_TE%_TF;if(_TE<=0){if(_TE>=0){return E(_TG);}else{if(_TF<=0){return E(_TG);}else{var _TH=E(_TG);return _TH==0?0:_TH+_TF|0;}}}else{if(_TF>=0){if(_TE>=0){return E(_TG);}else{if(_TF<=0){return E(_TG);}else{var _TI=E(_TG);return _TI==0?0:_TI+_TF|0;}}}else{var _TJ=E(_TG);return _TJ==0?0:_TJ+_TF|0;}}},_TK=new T(function(){return B(_a("(function(s){return md51(s.join(\',\'));})"));}),_TL=function(_TM,_){return new F(function(){return A(_TK,[E(_TM),_]);});},_TN=function(_TC,_){return new F(function(){return _TL(_TC,_);});},_TO=function(_TP){return new F(function(){return _6(function(_){var _=0;return new F(function(){return _TN(_TP,_);});});});},_TQ=function(_TR,_TS,_TT){while(1){var _TU=(function(_TV,_TW,_TX){if(_TV>_TW){var _TY=_TW,_TZ=_TV,_U0=_TX;_TR=_TY;_TS=_TZ;_TT=_U0;return null;}else{return [0,new T(function(){var _U1=(_TW-_TV|0)+1|0;switch(_U1){case -1:var _U2=[0,_TV];break;case 0:var _U2=E(_IO);break;default:var _U2=[0,B(_TD(B(_6(function(_){var _=0;return new F(function(){return _TB(_TX,_);});}))[1],_U1))+_TV|0];}var _U3=_U2;return _U3;}),new T(function(){return B(_TO(_TX));})];}})(_TR,_TS,_TT);if(_TU!=null){return _TU;}}},_U4=function(_U5){var _U6=new T(function(){var _U7=B(_TQ(0,2147483646,_U5));return [0,_U7[1],_U7[2]];});return [0,new T(function(){return E(E(_U6)[1]);}),new T(function(){return E(E(_U6)[2]);})];},_U8=function(_U9){return [0,_U9,new T(function(){return B(_TO(_U9));})];},_Ua=[0,_U4,_Tq,_U8],_Ub=function(_Uc){var _Ud=E(_Uc);return new F(function(){return _I1(_Ud[1],_Ud[2]);});},_Ue=function(_Uf){return [0,1/E(_Uf)[1]];},_Ug=function(_Uh){var _Ui=E(_Uh),_Uj=_Ui[1];return _Uj<0?[0, -_Uj]:E(_Ui);},_Uk=function(_Ul){var _Um=E(_Ul);return _Um[0]==0?_Um[1]:I_toNumber(_Um[1]);},_Un=function(_Uo){return [0,B(_Uk(_Uo))];},_Up=[0,0],_Uq=[0,1],_Ur=[0,-1],_Us=function(_Ut){var _Uu=E(E(_Ut)[1]);return _Uu==0?E(_Up):_Uu<=0?E(_Ur):E(_Uq);},_Uv=function(_Uw,_Ux){return [0,E(_Uw)[1]-E(_Ux)[1]];},_Uy=function(_Uz){return [0, -E(_Uz)[1]];},_UA=function(_UB,_UC){return [0,E(_UB)[1]+E(_UC)[1]];},_UD=function(_UE,_UF){return [0,E(_UE)[1]*E(_UF)[1]];},_UG=[0,_UA,_UD,_Uv,_Uy,_Ug,_Us,_Un],_UH=function(_UI,_UJ){return [0,E(_UI)[1]/E(_UJ)[1]];},_UK=[0,_UG,_UH,_Ue,_Ub],_UL=function(_UM,_UN){return E(_UM)[1]!=E(_UN)[1]?true:false;},_UO=function(_UP,_UQ){return E(_UP)[1]==E(_UQ)[1];},_UR=[0,_UO,_UL],_US=function(_UT,_UU){return E(_UT)[1]<E(_UU)[1];},_UV=function(_UW,_UX){return E(_UW)[1]<=E(_UX)[1];},_UY=function(_UZ,_V0){return E(_UZ)[1]>E(_V0)[1];},_V1=function(_V2,_V3){return E(_V2)[1]>=E(_V3)[1];},_V4=function(_V5,_V6){var _V7=E(_V5)[1],_V8=E(_V6)[1];return _V7>=_V8?_V7!=_V8?2:1:0;},_V9=function(_Va,_Vb){var _Vc=E(_Va),_Vd=E(_Vb);return _Vc[1]>_Vd[1]?E(_Vc):E(_Vd);},_Ve=function(_Vf,_Vg){var _Vh=E(_Vf),_Vi=E(_Vg);return _Vh[1]>_Vi[1]?E(_Vi):E(_Vh);},_Vj=[0,_UR,_V4,_US,_V1,_UY,_UV,_V9,_Ve],_Vk=function(_Vl,_Vm){var _Vn=hs_timesInt64(E(_Vl)[1],E(_Vm)[1]),_Vo=_Vn;return [0,_Vo];},_Vp=function(_Vq,_Vr){var _Vs=hs_plusInt64(E(_Vq)[1],E(_Vr)[1]),_Vt=_Vs;return [0,_Vt];},_Vu=function(_Vv,_Vw){var _Vx=hs_minusInt64(E(_Vv)[1],E(_Vw)[1]),_Vy=_Vx;return [0,_Vy];},_Vz=function(_VA){var _VB=hs_geInt64(_VA,new Long(0,0)),_VC=_VB;if(!E(_VC)){var _VD=hs_negateInt64(_VA),_VE=_VD;return E(_VE);}else{return E(_VA);}},_VF=function(_VG){return [0,B(_Vz(E(_VG)[1]))];},_VH=function(_VI){var _VJ=hs_intToInt64(_VI),_VK=_VJ;return E(_VK);},_VL=function(_VM){var _VN=E(_VM);return _VN[0]==0?B(_VH(_VN[1])):I_toInt64(_VN[1]);},_VO=function(_VP){return [0,B(_VL(_VP))];},_VQ=function(_VR){var _VS=hs_negateInt64(E(_VR)[1]),_VT=_VS;return [0,_VT];},_VU=[0,new Long(1,0)],_VV=new T(function(){var _VW=hs_negateInt64(new Long(1,0)),_VX=_VW;return [0,_VX];}),_VY=[0,new Long(0,0)],_VZ=function(_W0){var _W1=hs_gtInt64(_W0,new Long(0,0)),_W2=_W1;if(!E(_W2)){var _W3=hs_eqInt64(_W0,new Long(0,0)),_W4=_W3;return E(_W4)==0?E(_VV):E(_VY);}else{return E(_VU);}},_W5=function(_W6){return new F(function(){return _VZ(E(_W6)[1]);});},_W7=[0,_Vp,_Vk,_Vu,_VQ,_VF,_W5,_VO],_W8=[1,I_fromBits([4294967295,2147483647])],_W9=new T(function(){var _Wa=hs_negateInt64(new Long(0,-2147483648)),_Wb=_Wa;return [0,_Wb];}),_Wc=function(_Wd){var _We=hs_intToInt64(2147483647),_Wf=_We,_Wg=hs_leInt64(_Wd,_Wf),_Wh=_Wg;if(!E(_Wh)){return [1,I_fromInt64(_Wd)];}else{var _Wi=hs_intToInt64(-2147483648),_Wj=_Wi,_Wk=hs_geInt64(_Wd,_Wj),_Wl=_Wk;if(!E(_Wl)){return [1,I_fromInt64(_Wd)];}else{var _Wm=hs_int64ToInt(_Wd),_Wn=_Wm;return new F(function(){return _q9(_Wn);});}}},_Wo=new T(function(){return B(_Wc(E(_W9)[1]));}),_Wp=[0,new Long(2,0)],_Wq=[0,new Long(1,0)],_Wr=new T(function(){return B(unCStr("Negative exponent"));}),_Ws=new T(function(){return B(err(_Wr));}),_Wt=0,_Wu=new T(function(){return B(_IL(_Wt));}),_Wv=new T(function(){return die(_Wu);}),_Ww=function(_Wx,_Wy){var _Wz=hs_eqInt64(_Wy,new Long(0,0)),_WA=_Wz;if(!E(_WA)){var _WB=hs_eqInt64(_Wy,E(_VV)[1]),_WC=_WB;if(!E(_WC)){var _WD=hs_quotInt64(_Wx,_Wy),_WE=_WD;return E(_WE);}else{var _WF=hs_eqInt64(_Wx,E(_W9)[1]),_WG=_WF;if(!E(_WG)){var _WH=hs_quotInt64(_Wx,_Wy),_WI=_WH;return E(_WI);}else{return E(_Wv);}}}else{return E(_IO);}},_WJ=function(_WK,_WL){var _WM=hs_eqInt64(_WL,new Long(0,0)),_WN=_WM;if(!E(_WN)){var _WO=hs_eqInt64(_WL,E(_VV)[1]),_WP=_WO;if(!E(_WP)){var _WQ=hs_remInt64(_WK,_WL),_WR=_WQ;return E(_WR);}else{return new F(function(){return new Long(0,0);});}}else{return E(_IO);}},_WS=function(_WT,_WU,_WV){while(1){var _WW=(function(_WX,_WY,_WZ){var _X0=hs_eqInt64(B(_WJ(_WY,new Long(2,0))),new Long(0,0)),_X1=_X0;if(!E(_X1)){var _X2=hs_eqInt64(_WY,new Long(1,0)),_X3=_X2;if(!E(_X3)){var _X4=hs_minusInt64(_WY,new Long(1,0)),_X5=_X4;_WT=new T(function(){return B(_Vk(_WX,_WX));});_WU=B(_Ww(_X5,new Long(2,0)));_WV=new T(function(){return B(_Vk(_WX,_WZ));},1);return null;}else{var _X6=hs_timesInt64(E(_WX)[1],E(_WZ)[1]),_X7=_X6;return E(_X7);}}else{_WT=new T(function(){return B(_Vk(_WX,_WX));});var _X8=B(_Ww(_WY,new Long(2,0))),_X9=_WZ;_WU=_X8;_WV=_X9;return null;}})(_WT,_WU,_WV);if(_WW!=null){return _WW;}}},_Xa=function(_Xb,_Xc){while(1){var _Xd=(function(_Xe,_Xf){var _Xg=hs_eqInt64(B(_WJ(_Xf,new Long(2,0))),new Long(0,0)),_Xh=_Xg;if(!E(_Xh)){var _Xi=hs_eqInt64(_Xf,new Long(1,0)),_Xj=_Xi;if(!E(_Xj)){var _Xk=hs_minusInt64(_Xf,new Long(1,0)),_Xl=_Xk;return [0,B(_WS(new T(function(){return B(_Vk(_Xe,_Xe));}),B(_Ww(_Xl,new Long(2,0))),_Xe))];}else{return E(_Xe);}}else{_Xb=new T(function(){return B(_Vk(_Xe,_Xe));});var _Xm=B(_Ww(_Xf,new Long(2,0)));_Xc=_Xm;return null;}})(_Xb,_Xc);if(_Xd!=null){return _Xd;}}},_Xn=function(_Xo,_Xp){var _Xq=hs_ltInt64(_Xp,new Long(0,0)),_Xr=_Xq;if(!E(_Xr)){var _Xs=hs_eqInt64(_Xp,new Long(0,0)),_Xt=_Xs;return E(_Xt)==0?B(_Xa(_Xo,_Xp)):E(_Wq);}else{return E(_Ws);}},_Xu=new T(function(){return B(_Xn(_Wp,new Long(53,0)));}),_Xv=new T(function(){return [0,B(_Uk(B(_Wc(E(_Xu)[1]))))];}),_Xw=new T(function(){var _Xx=hs_minusInt64(E(_Xu)[1],new Long(1,0)),_Xy=_Xx;return [0,_Xy];}),_Xz=function(_XA,_XB){var _XC=hs_int64ToWord64(_XB),_XD=_XC,_XE=hs_int64ToWord64(_XA),_XF=_XE,_XG=hs_and64(_XF,_XD),_XH=_XG,_XI=hs_word64ToInt64(_XH),_XJ=_XI;return E(_XJ);},_XK=[0,1],_XL=function(_XM,_XN){return [0,E(_XM)[1],E(_XN)[1]];},_XO=function(_XP,_XQ){var _XR=quot(_XQ,52774),_XS=(imul(40692,_XQ-(imul(_XR,52774)|0)|0)|0)-(imul(_XR,3791)|0)|0,_XT=new T(function(){if(_XS>=0){var _XU=[0,_XS];}else{var _XU=[0,_XS+2147483399|0];}var _XV=_XU;return _XV;}),_XW=quot(_XP,53668),_XX=(imul(40014,_XP-(imul(_XW,53668)|0)|0)|0)-(imul(_XW,12211)|0)|0,_XY=new T(function(){if(_XX>=0){var _XZ=[0,_XX];}else{var _XZ=[0,_XX+2147483563|0];}var _Y0=_XZ;return _Y0;});return [0,new T(function(){var _Y1=E(_XY)[1]-E(_XT)[1]|0;if(_Y1>=1){var _Y2=[0,_Y1];}else{var _Y2=[0,_Y1+2147483562|0];}var _Y3=_Y2,_Y4=_Y3,_Y5=_Y4,_Y6=_Y5;return _Y6;}),new T(function(){return B(_XL(_XY,_XT));})];},_Y7=[0,2147483562],_Y8=function(_Y9){return E(E(_Y9)[7]);},_Ya=[0,0],_Yb=[0,1000],_Yc=function(_Yd,_Ye){while(1){var _Yf=E(_Yd);if(!_Yf[0]){var _Yg=E(_Yf[1]);if(_Yg==(-2147483648)){_Yd=[1,I_fromInt(-2147483648)];continue;}else{var _Yh=E(_Ye);if(!_Yh[0]){return [0,B(_TD(_Yg,_Yh[1]))];}else{_Yd=[1,I_fromInt(_Yg)];_Ye=_Yh;continue;}}}else{var _Yi=_Yf[1],_Yj=E(_Ye);return _Yj[0]==0?[0,I_toInt(I_mod(_Yi,I_fromInt(_Yj[1])))]:[1,I_mod(_Yi,_Yj[1])];}}},_Yk=function(_Yl,_Ym,_Yn,_Yo){while(1){var _Yp=(function(_Yq,_Yr,_Ys,_Yt){if(!B(_Kw(_Yr,_Ys))){var _Yu=B(_pT(B(_Jv(_Ys,_Yr)),_XK)),_Yv=B((function(_Yw,_Yx,_Yy){while(1){if(!B(_K9(_Yw,B(_qb(_Yu,_Yb))))){var _Yz=E(_Yy),_YA=B(_XO(_Yz[1],_Yz[2])),_YB=B(_qb(_Yw,_Y7)),_YC=B(_pT(B(_qb(_Yx,_Y7)),B(_Jv(B(_q9(E(_YA[1])[1])),_XK))));_Yy=_YA[2];_Yw=_YB;_Yx=_YC;continue;}else{return [0,_Yx,_Yy];}}})(_XK,_Ya,_Yt));return [0,new T(function(){return B(A(_Y8,[_Yq,new T(function(){if(!B(_HL(_Yu,_Ya))){var _YD=B(_pT(_Yr,B(_Yc(_Yv[1],_Yu))));}else{var _YD=E(_IO);}return _YD;})]));}),_Yv[2]];}else{var _YE=_Yq,_YF=_Ys,_YG=_Yr,_YH=_Yt;_Yl=_YE;_Ym=_YF;_Yn=_YG;_Yo=_YH;return null;}})(_Yl,_Ym,_Yn,_Yo);if(_Yp!=null){return _Yp;}}},_YI=function(_YJ){var _YK=B(_Yk(_W7,_Wo,_W8,_YJ));return [0,new T(function(){return [0,B(_Uk(B(_Wc(B(_Xz(E(_Xw)[1],E(_YK[1])[1]))))))/E(_Xv)[1]];}),_YK[2]];},_YL=function(_YM){var _YN=new T(function(){var _YO=B(_YI(_YM));return [0,_YO[2],_YO[1]];}),_YP=new T(function(){return E(E(_YN)[1]);});return [0,_YP,new T(function(){var _YQ=E(_YP);return E(E(_YN)[2]);})];},_YR=[1,I_fromBits([3567587328,232])],_YS=function(_YT,_YU,_YV,_YW){var _YX=B(_qb(_YU,_YV));return new F(function(){return _Jm(B(_qb(B(_qb(_YT,_YW)),B(_JW(_YX)))),B(_J6(_YX)));});},_YY=[0,0],_YZ=function(_Z0,_Z1){var _Z2=E(_Z1);if(!_Z2){return E(_IO);}else{var _Z3=function(_Z4){if(_Z0<=0){if(_Z0>=0){var _Z5=quotRemI(_Z0,_Z2);return [0,[0,_Z5[1]],[0,_Z5[2]]];}else{if(_Z2<=0){var _Z6=quotRemI(_Z0,_Z2);return [0,[0,_Z6[1]],[0,_Z6[2]]];}else{var _Z7=quotRemI(_Z0+1|0,_Z2);return [0,[0,_Z7[1]-1|0],[0,(_Z7[2]+_Z2|0)-1|0]];}}}else{if(_Z2>=0){if(_Z0>=0){var _Z8=quotRemI(_Z0,_Z2);return [0,[0,_Z8[1]],[0,_Z8[2]]];}else{if(_Z2<=0){var _Z9=quotRemI(_Z0,_Z2);return [0,[0,_Z9[1]],[0,_Z9[2]]];}else{var _Za=quotRemI(_Z0+1|0,_Z2);return [0,[0,_Za[1]-1|0],[0,(_Za[2]+_Z2|0)-1|0]];}}}else{var _Zb=quotRemI(_Z0-1|0,_Z2);return [0,[0,_Zb[1]-1|0],[0,(_Zb[2]+_Z2|0)+1|0]];}}};return E(_Z2)==(-1)?E(_Z0)==(-2147483648)?[0,_Wv,_YY]:B(_Z3(_)):B(_Z3(_));}},_Zc=function(_Zd){var _Ze=B(_YZ((_Zd>>>0&2147483647>>>0)>>>0&4294967295,2147483562));return [0,E(_Ze[2])[1]+1|0,B(_TD(E(_Ze[1])[1],2147483398))+1|0];},_Zf=function(_Zg){return E(_YR);},_Zh=[0,1],_Zi=function(_Zj,_Zk){var _Zl=E(_Zj);return [0,_Zl,new T(function(){var _Zm=B(_Zi(B(_pT(_Zl,_Zk)),_Zk));return [1,_Zm[1],_Zm[2]];})];},_Zn=function(_Zo){var _Zp=B(_Zi(_Zo,_Zh));return [1,_Zp[1],_Zp[2]];},_Zq=function(_Zr,_Zs){var _Zt=B(_Zi(_Zr,new T(function(){return B(_Jv(_Zs,_Zr));})));return [1,_Zt[1],_Zt[2]];},_Zu=[0,0],_Zv=function(_Zw,_Zx,_Zy){if(!B(_K9(_Zx,_Zu))){var _Zz=function(_ZA){return !B(_HT(_ZA,_Zy))?[1,_ZA,new T(function(){return B(_Zz(B(_pT(_ZA,_Zx))));})]:[0];};return new F(function(){return _Zz(_Zw);});}else{var _ZB=function(_ZC){return !B(_Kw(_ZC,_Zy))?[1,_ZC,new T(function(){return B(_ZB(B(_pT(_ZC,_Zx))));})]:[0];};return new F(function(){return _ZB(_Zw);});}},_ZD=function(_ZE,_ZF,_ZG){return new F(function(){return _Zv(_ZE,B(_Jv(_ZF,_ZE)),_ZG);});},_ZH=function(_ZI,_ZJ){return new F(function(){return _Zv(_ZI,_Zh,_ZJ);});},_ZK=function(_ZL){return [0,B(_ro(_ZL))];},_ZM=function(_ZN){return new F(function(){return _Jv(_ZN,_Zh);});},_ZO=function(_ZP){return new F(function(){return _pT(_ZP,_Zh);});},_ZQ=function(_ZR){return new F(function(){return _q9(E(_ZR)[1]);});},_ZS=[0,_ZO,_ZM,_ZQ,_ZK,_Zn,_Zq,_ZH,_ZD],_ZT=function(_ZU,_ZV){if(_ZU<=0){if(_ZU>=0){return new F(function(){return quot(_ZU,_ZV);});}else{if(_ZV<=0){return new F(function(){return quot(_ZU,_ZV);});}else{return quot(_ZU+1|0,_ZV)-1|0;}}}else{if(_ZV>=0){if(_ZU>=0){return new F(function(){return quot(_ZU,_ZV);});}else{if(_ZV<=0){return new F(function(){return quot(_ZU,_ZV);});}else{return quot(_ZU+1|0,_ZV)-1|0;}}}else{return quot(_ZU-1|0,_ZV)-1|0;}}},_ZW=function(_ZX,_ZY){while(1){var _ZZ=E(_ZX);if(!_ZZ[0]){var _100=E(_ZZ[1]);if(_100==(-2147483648)){_ZX=[1,I_fromInt(-2147483648)];continue;}else{var _101=E(_ZY);if(!_101[0]){return [0,B(_ZT(_100,_101[1]))];}else{_ZX=[1,I_fromInt(_100)];_ZY=_101;continue;}}}else{var _102=_ZZ[1],_103=E(_ZY);return _103[0]==0?[0,I_toInt(I_div(_102,I_fromInt(_103[1])))]:[1,I_div(_102,_103[1])];}}},_104=function(_105,_106){return !B(_HL(_106,_IP))?B(_ZW(_105,_106)):E(_IO);},_107=function(_108,_109){while(1){var _10a=E(_108);if(!_10a[0]){var _10b=E(_10a[1]);if(_10b==(-2147483648)){_108=[1,I_fromInt(-2147483648)];continue;}else{var _10c=E(_109);if(!_10c[0]){var _10d=_10c[1];return [0,[0,B(_ZT(_10b,_10d))],[0,B(_TD(_10b,_10d))]];}else{_108=[1,I_fromInt(_10b)];_109=_10c;continue;}}}else{var _10e=E(_109);if(!_10e[0]){_108=_10a;_109=[1,I_fromInt(_10e[1])];continue;}else{var _10f=I_divMod(_10a[1],_10e[1]);return [0,[1,_10f[1]],[1,_10f[2]]];}}}},_10g=function(_10h,_10i){if(!B(_HL(_10i,_IP))){var _10j=B(_107(_10h,_10i));return [0,_10j[1],_10j[2]];}else{return E(_IO);}},_10k=function(_10l,_10m){return !B(_HL(_10m,_IP))?B(_Yc(_10l,_10m)):E(_IO);},_10n=function(_10o,_10p){return !B(_HL(_10p,_IP))?B(_Jb(_10o,_10p)):E(_IO);},_10q=function(_10r,_10s){while(1){var _10t=E(_10r);if(!_10t[0]){var _10u=E(_10t[1]);if(_10u==(-2147483648)){_10r=[1,I_fromInt(-2147483648)];continue;}else{var _10v=E(_10s);if(!_10v[0]){var _10w=_10v[1];return [0,[0,quot(_10u,_10w)],[0,_10u%_10w]];}else{_10r=[1,I_fromInt(_10u)];_10s=_10v;continue;}}}else{var _10x=E(_10s);if(!_10x[0]){_10r=_10t;_10s=[1,I_fromInt(_10x[1])];continue;}else{var _10y=I_quotRem(_10t[1],_10x[1]);return [0,[1,_10y[1]],[1,_10y[2]]];}}}},_10z=function(_10A,_10B){if(!B(_HL(_10B,_IP))){var _10C=B(_10q(_10A,_10B));return [0,_10C[1],_10C[2]];}else{return E(_IO);}},_10D=function(_10E){return E(_10E);},_10F=function(_10G){return E(_10G);},_10H=[0,_pT,_qb,_Jv,_q3,_J6,_JW,_10F],_10I=function(_10J,_10K){var _10L=E(_10J);if(!_10L[0]){var _10M=_10L[1],_10N=E(_10K);return _10N[0]==0?_10M!=_10N[1]:I_compareInt(_10N[1],_10M)==0?false:true;}else{var _10O=_10L[1],_10P=E(_10K);return _10P[0]==0?I_compareInt(_10O,_10P[1])==0?false:true:I_compare(_10O,_10P[1])==0?false:true;}},_10Q=[0,_HL,_10I],_10R=function(_10S,_10T){return !B(_rr(_10S,_10T))?E(_10S):E(_10T);},_10U=function(_10V,_10W){return !B(_rr(_10V,_10W))?E(_10W):E(_10V);},_10X=function(_10Y,_10Z){var _110=E(_10Y);if(!_110[0]){var _111=_110[1],_112=E(_10Z);if(!_112[0]){var _113=_112[1];return _111!=_113?_111>_113?2:0:1;}else{var _114=I_compareInt(_112[1],_111);return _114<=0?_114>=0?1:2:0;}}else{var _115=_110[1],_116=E(_10Z);if(!_116[0]){var _117=I_compareInt(_115,_116[1]);return _117>=0?_117<=0?1:2:0;}else{var _118=I_compare(_115,_116[1]);return _118>=0?_118<=0?1:2:0;}}},_119=[0,_10Q,_10X,_HT,_K9,_Kw,_rr,_10R,_10U],_11a=[0,_10H,_119,_Ta],_11b=[0,_11a,_ZS,_10n,_IY,_104,_10k,_10z,_10g,_10D],_11c=[0,0],_11d=function(_11e,_11f,_11g){var _11h=B(A(_11e,[_11f]));if(!B(_HL(_11h,_11c))){return new F(function(){return _ZW(B(_qb(_11f,_11g)),_11h);});}else{return E(_IO);}},_11i=function(_11j){return E(E(_11j)[1]);},_11k=function(_11l){return E(E(_11l)[1]);},_11m=function(_11n,_11o,_11p){var _11q=new T(function(){if(!B(_HL(_11p,_IP))){var _11r=B(_10q(_11o,_11p)),_11s=[0,_11r[1],_11r[2]];}else{var _11s=E(_IO);}return _11s;});return [0,new T(function(){return B(A(_Y8,[B(_11k(B(_11i(_11n)))),new T(function(){return E(E(_11q)[1]);})]));}),new T(function(){return [0,E(E(E(_11q)[2])),E(_11p)];})];},_11t=function(_11u,_11v,_11w){var _11x=B(_11m(_11u,_11v,_11w)),_11y=_11x[1],_11z=E(_11x[2]);if(!B(_HT(B(_qb(_11z[1],_Jq)),B(_qb(_IP,_11z[2]))))){return E(_11y);}else{var _11A=E(B(_11i(_11u))[1]);return new F(function(){return A(_11A[3],[_11y,new T(function(){return B(A(_11A[7],[_Jq]));})]);});}},_11B=[1,I_fromBits([2627207168,20116567])],_11C=[0,40587],_11D=function(_11E){var _11F=new T(function(){var _11G=B(_YS(E(_11E),_Jq,_YR,_Jq)),_11H=B(_YS(_11B,_Jq,_YR,_Jq)),_11I=B(_YS(_11G[1],_11G[2],_11H[1],_11H[2]));return B(_11t(_11b,_11I[1],_11I[2]));});return [0,new T(function(){return B(_pT(_11C,_11F));}),new T(function(){return B(_Jv(_11E,B(_11d(_Zf,B(_qb(_11F,_YR)),_11B))));})];},_11J=[0,0],_11K=function(_11L,_11M,_){var _=writeOffAddr("w32",4,E(_11L)[1],0,E(_11M)[1]);return _c;},_11N=function(_11O,_){var _11P=readOffAddr("w32",4,E(_11O)[1],0),_11Q=_11P;return [0,_11Q];},_11R=function(_11S,_11T,_11U,_){var _=writeOffAddr("w32",4,plusAddr(E(_11S)[1],E(_11T)[1]),0,E(_11U)[1]);return _c;},_11V=function(_11W,_11X,_){var _11Y=readOffAddr("w32",4,plusAddr(E(_11W)[1],E(_11X)[1]),0),_11Z=_11Y;return [0,_11Z];},_120=[0,4],_121=function(_122){return E(_120);},_123=function(_124,_125,_){var _126=readOffAddr("w32",4,E(_124)[1],E(_125)[1]),_127=_126;return [0,_127];},_128=function(_129,_12a,_12b,_){var _=writeOffAddr("w32",4,E(_129)[1],E(_12a)[1],E(_12b)[1]);return _c;},_12c=[0,_121,_121,_123,_128,_11V,_11R,_11N,_11K],_12d=[0,0],_12e=function(_12f){return E(E(_12f)[3]);},_12g=function(_12h,_12i,_12j,_){if(_12i>0){return new F(function(){return (function(_12k,_12l,_){while(1){var _12m=E(_12k);if(!_12m){var _12n=B(A(new T(function(){return B(A(_12e,[_12h,_12j,_12d]));}),[_])),_12o=_12n;return [1,_12o,_12l];}else{var _12p=B(A(new T(function(){return B(_12e(_12h));}),[_12j,[0,_12m],_])),_12q=_12p;_12k=_12m-1|0;var _12r=[1,_12q,_12l];_12l=_12r;continue;}}})(_12i-1|0,_1g,_);});}else{return _1g;}},_12s=0,_12t=1,_12u=function(_12v,_12w,_12x,_){var _12y=0,_12z=_12y;switch(E(_12z)){case 0:return new F(function(){return (function(_){var _12A=B(A(_12v,[_])),_12B=_12A,_12C=jsCatch(function(_){return new F(function(){return new T(function(){return B(A(_12x,[_12B]));})();});},function(_12D,_){var _12E=B(A(_12w,[_12B,_])),_12F=_12E;return new F(function(){return die(_12D);});}),_12G=_12C,_12H=B(A(_12w,[_12B,_])),_12I=_12H;return _12G;})();});break;case 1:var _12J=B(A(_12v,[_])),_12K=_12J,_12L=jsCatch(new T(function(){return B(A(_12x,[_12K]));}),function(_12M,_){var _12N=B(A(_12w,[_12K,_])),_12O=_12N;return new F(function(){return die(_12M);});}),_12P=_12L,_12Q=B(A(_12w,[_12K,_])),_12R=_12Q;return _12P;default:var _12S=B(A(_12v,[_])),_12T=_12S,_12U=jsCatch(new T(function(){return B(A(_12x,[_12T]));}),function(_12V,_){var _12W=B(A(_12w,[_12T,_])),_12X=_12W;return new F(function(){return die(_12V);});}),_12Y=_12U,_12Z=B(A(_12w,[_12T,_])),_130=_12Z;return _12Y;}},_131=function(_132){return E(E(_132)[3]);},_133=0,_134=[0,_133,_1g],_135=new T(function(){return B(unCStr("mallocForeignPtrBytes: size must be >= 0"));}),_136=new T(function(){return B(err(_135));}),_137=function(_138,_139,_){var _13a=B((function(_13b,_){while(1){var _13c=readOffAddr("i8",1,_139,_13b),_13d=_13c;if(!E(_13d)){return [0,_13b];}else{var _13e=_13b+1|0;_13b=_13e;continue;}}})(0,_)),_13f=_13a;return new F(function(){return _12u(E(_138)[2],_131,function(_13g,_){var _13h=nMV(_134),_13i=_13h,_13j=E(_13f)[1],_13k=function(_13l){var _13m=imul(_13l,4)|0;if(_13m>=0){var _13n=nMV(_134),_13o=_13n,_13p=newByteArr(_13m),_13q=_13p,_13r=function(_13s,_){var _13t=E(_13g),_13u=B(A(_13t[1],[_13s,[0,_13q,[1,_13q,_13o],_12t,_13l,0,0],_])),_13v=_13u,_13w=E(_13v),_13x=_13w[3],_13y=E(_13w[2]);if(_13y[5]!=_13y[6]){if(E(_13w[1])==1){var _13z=E(_13x),_13A=_13z[2],_13B=B(_12g(_12c,_13z[6]-_13z[5]|0,[0,_13z[1]],_)),_13C=_13B,_=0,_13D=B(_13r(_13y,_)),_13E=_13D;return new T(function(){return B(_5B(_13C,_13E));});}else{var _13F=B(A(_13t[2],[_13y,_13x,_])),_13G=_13F,_13H=E(_13G),_13I=E(_13H[2]),_13J=_13I[2],_13K=B(_12g(_12c,_13I[6]-_13I[5]|0,[0,_13I[1]],_)),_13L=_13K,_=0,_13M=B(_13r(_13H[1],_)),_13N=_13M;return new T(function(){return B(_5B(_13L,_13N));});}}else{var _13O=E(_13x),_13P=_13O[2],_13Q=B(_12g(_12c,_13O[6]-_13O[5]|0,[0,_13O[1]],_)),_13R=_13Q,_=0;return _13R;}};return new F(function(){return _13r([0,_139,[0,_13i],_12s,_13j,0,_13j],_);});}else{return E(_136);}};return _13j>1?B(_13k(_13j)):B(_13k(1));},_);});},_13S=1,_13T=new T(function(){return B(unCStr("UTF16LE"));}),_13U=new T(function(){return B(unCStr("UTF16BE"));}),_13V=new T(function(){return B(unCStr("UTF16"));}),_13W=new T(function(){return B(unCStr("UTF8"));}),_13X=new T(function(){return B(unCStr("UTF32LE"));}),_13Y=new T(function(){return B(unCStr("UTF32BE"));}),_13Z=new T(function(){return B(unCStr("UTF32"));}),_140=function(_141){var _142=u_towupper(_141),_143=_142;return _143>>>0>1114111?B(_rm(_143)):_143;},_144=function(_145){while(1){var _146=(function(_147){var _148=E(_147);if(!_148[0]){return [0];}else{var _149=_148[2],_14a=E(E(_148[1])[1]);if(_14a==45){_145=_149;return null;}else{return [1,new T(function(){return [0,B(_140(_14a))];}),new T(function(){return B(_144(_149));})];}}})(_145);if(_146!=null){return _146;}}},_14b=new T(function(){return B(unCStr("UTF-32LE"));}),_14c=0,_14d=1,_14e=new T(function(){return [0, -(1&4294967295)>>>0];}),_14f=[0,0],_14g=function(_14h,_){return new F(function(){return die(new T(function(){return B(_n8(_14h));}));});},_14i=function(_14j,_){return new F(function(){return _14g(_14j,_);});},_14k=new T(function(){return B(unCStr("iconvRecoder"));}),_14l=[0,-1],_14m=function(_14n,_14o,_14p,_14q,_14r,_14s,_14t,_14u,_14v,_14w,_14x,_14y,_14z,_14A,_14B,_){var _14C=newByteArr(4),_14D=_14C,_14E=_14D,_14F=_14E,_14G=E(_14u)[1],_14H=function(_14I){var _14J=plusAddr(_14o,_14I),_=die("Unsupported PrimOp: writeAddrOffAddr#"),_14K=newByteArr(4),_14L=_14K,_14M=_14L,_14N=_14M,_14O=E(_14B)[1],_14P=function(_14Q){var _14R=plusAddr(_14v,_14Q),_=die("Unsupported PrimOp: writeAddrOffAddr#"),_14S=newByteArr(4),_14T=_14S,_14U=_14T,_14V=_14U,_14W=function(_14X){var _14Y=_14V,_=writeOffAddr("w32",4,_14Y,0,_14X),_14Z=newByteArr(4),_150=_14Z,_151=_150,_152=_151,_153=function(_154){var _155=_152,_=writeOffAddr("w32",4,_155,0,_154),_156=hs_iconv(E(_14n)[1],_14F,_14Y,_14N,_155),_157=_156,_158=readOffAddr("w32",4,_14Y,0),_159=_158,_15a=readOffAddr("w32",4,_155,0),_15b=_15a,_15c=new T(function(){if(_14O<32){var _15d=[0,(_15b&4294967295)>>_14O];}else{var _15d=(_15b&4294967295)>=0?E(_14f):E(_14l);}var _15e=_15d;return _15e;}),_15f=new T(function(){var _15g=E(_159);if(!_15g){var _15h=[0,_14o,_14p,_14q,_14r,0,0];}else{if(_14G<32){var _15i=[0,_14o,_14p,_14q,_14r,_14t-((_15g&4294967295)>>_14G)|0,_14t];}else{if((_15g&4294967295)>=0){var _15j=[0,_14o,_14p,_14q,_14r,_14t,_14t];}else{var _15j=[0,_14o,_14p,_14q,_14r,_14t+1|0,_14t];}var _15k=_15j,_15l=_15k,_15i=_15l;}var _15m=_15i,_15h=_15m;}return _15h;});if(_157!=E(_14e)[1]){var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_14c,_15f,new T(function(){return [0,_14v,_14w,_14x,_14y,_14z,_14y-E(_15c)[1]|0];})];}else{var _15n=__hscore_get_errno(),_15o=_15n;switch(E(_15o)){case 7:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_14d,_15f,new T(function(){return [0,_14v,_14w,_14x,_14y,_14z,_14y-E(_15c)[1]|0];})];case 22:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,_14c,_15f,new T(function(){return [0,_14v,_14w,_14x,_14y,_14z,_14y-E(_15c)[1]|0];})];case 84:var _=0,_=0,_=0,_=0,_=0,_=0;return [0,new T(function(){return E(E(_15c)[1])==0?1:2;}),_15f,new T(function(){return [0,_14v,_14w,_14x,_14y,_14z,_14y-E(_15c)[1]|0];})];default:var _15p=__hscore_get_errno(),_15q=_15p;return new F(function(){return _14i(B(_15r(_14k,_15q,_5A,_5A)),_);});}}};if(_14O<32){return new F(function(){return _153((_14y-_14A|0)<<_14O>>>0);});}else{return new F(function(){return _153(0);});}};if(_14G<32){return new F(function(){return _14W((_14t-_14s|0)<<_14G>>>0);});}else{return new F(function(){return _14W(0);});}};if(_14O<32){return new F(function(){return _14P(_14A<<_14O);});}else{return new F(function(){return _14P(0);});}};if(_14G<32){return new F(function(){return _14H(_14s<<_14G);});}else{return new F(function(){return _14H(0);});}},_15s=[0,2],_15t=function(_15u,_15v,_15w,_){var _15x=E(_15v),_15y=E(_15w);return new F(function(){return _14m(_15u,_15x[1],_15x[2],_15x[3],_15x[4],_15x[5],_15x[6],_15s,_15y[1],_15y[2],_15y[3],_15y[4],_15y[5],_15y[6],_14f,_);});},_15z=function(_15A,_15B,_15C,_){var _15D=E(_15B),_15E=E(_15C);return new F(function(){return _14m(_15A,_15D[1],_15D[2],_15D[3],_15D[4],_15D[5],_15D[6],_14f,_15E[1],_15E[2],_15E[3],_15E[4],_15E[5],_15E[6],_15s,_);});},_15F=function(_15G){return E(E(_15G)[1])==47?false:true;},_15H=function(_15I,_){return _c;},_15J=function(_){return _c;},_15K=new T(function(){return B(unCStr("mkTextEncoding"));}),_15L=new T(function(){return B(unCStr("Iconv.close"));}),_15M=function(_15N,_15O,_){var _15P=newByteArr(B(_I4(_15N,0))+1|0),_15Q=_15P,_15R=_15Q,_15S=_15R,_15T=_15S,_15U=B((function(_15V,_15W,_){while(1){var _15X=E(_15V);if(!_15X[0]){var _=writeOffAddr("i8",1,_15T,_15W,0);return _c;}else{var _=writeOffAddr("i8",1,_15T,_15W,E(_15X[1])[1]&255);_15V=_15X[2];var _15Y=_15W+1|0;_15W=_15Y;continue;}}})(_15N,0,_)),_15Z=_15U,_160=B(A(_15O,[[0,_15T],_])),_161=_160,_=0;return _161;},_162=function(_163,_164,_){return new F(function(){return _15M(_163,_164,_);});},_165=function(_166,_167,_168,_169){return new F(function(){return _162(_166,function(_16a){return new F(function(){return _162(_167,function(_16b,_){var _16c=hs_iconv_open(E(_16b)[1],E(_16a)[1]),_16d=_16c,_16e=E(_16d);if(_16e==(-1)){var _16f=__hscore_get_errno(),_16g=_16f;return new F(function(){return _14i(B(_15r(_15K,_16g,_5A,_5A)),_);});}else{return [0,new T(function(){return B(A(_169,[[0,_16e]]));}),_168,function(_){var _16h=hs_iconv_close(_16e),_16i=_16h;if(E(_16i)==(-1)){var _16j=__hscore_get_errno(),_16k=_16j;return new F(function(){return _14i(B(_15r(_15L,_16k,_5A,_5A)),_);});}else{return _c;}},_15J,_15H];}});});});});},_16l=function(_14j,_){return new F(function(){return _14g(_14j,_);});},_16m=12,_16n=new T(function(){return B(unCStr("invalid byte sequence"));}),_16o=new T(function(){return B(unCStr("recoverDecode"));}),_16p=[0,_5A,_16m,_16o,_16n,_5A,_5A],_16q=function(_16r,_16s,_16t,_16u,_16v,_16w,_16x,_16y,_16z,_16A,_16B,_16C,_16D,_){switch(E(_16r)){case 0:return new F(function(){return _16l(_16p,_);});break;case 1:return [0,[0,_16s,_16t,_16u,_16v,_16w+1|0,_16x],[0,_16y,_16z,_16A,_16B,_16C,_16D]];case 2:var _=writeOffAddr("w32",4,_16y,_16D,65533),_=0;return [0,[0,_16s,_16t,_16u,_16v,_16w+1|0,_16x],[0,_16y,_16z,_16A,_16B,_16C,_16D+1|0]];default:var _16E=readOffAddr("w8",1,plusAddr(_16s,_16w),0),_16F=_16E,_=0;if(_16F>=128){var _16G=56320+(_16F&4294967295)|0;if(_16G>>>0>1114111){return new F(function(){return _rm(_16G);});}else{var _=writeOffAddr("w32",4,_16y,_16D,_16G),_=0;return [0,[0,_16s,_16t,_16u,_16v,_16w+1|0,_16x],[0,_16y,_16z,_16A,_16B,_16C,_16D+1|0]];}}else{var _16H=_16F&4294967295;if(_16H>>>0>1114111){return new F(function(){return _rm(_16H);});}else{var _=writeOffAddr("w32",4,_16y,_16D,_16H),_=0;return [0,[0,_16s,_16t,_16u,_16v,_16w+1|0,_16x],[0,_16y,_16z,_16A,_16B,_16C,_16D+1|0]];}}}},_16I=function(_16J,_16K,_16L,_){var _16M=E(_16K),_16N=E(_16L);return new F(function(){return _16q(_16J,_16M[1],_16M[2],_16M[3],_16M[4],_16M[5],_16M[6],_16N[1],_16N[2],_16N[3],_16N[4],_16N[5],_16N[6],_);});},_16O=new T(function(){return B(unCStr("recoverEncode"));}),_16P=new T(function(){return B(unCStr("invalid character"));}),_16Q=[0,_5A,_16m,_16O,_16P,_5A,_5A],_16R=function(_){return new F(function(){return _16l(_16Q,_);});},_16S=function(_16T,_16U,_16V,_16W,_16X,_16Y,_16Z,_170,_171,_172,_173,_174,_175,_){var _176=readOffAddr("w32",4,_16U,_16Y),_177=_176,_=0;switch(E(_16T)){case 0:return new F(function(){return _16R(_);});break;case 1:return [0,[0,_16U,_16V,_16W,_16X,_16Y+1|0,_16Z],[0,_170,_171,_172,_173,_174,_175]];case 2:if(E(_177)==63){return [0,[0,_16U,_16V,_16W,_16X,_16Y+1|0,_16Z],[0,_170,_171,_172,_173,_174,_175]];}else{var _=writeOffAddr("w32",4,_16U,_16Y,63),_=0;return [0,[0,_16U,_16V,_16W,_16X,_16Y,_16Z],[0,_170,_171,_172,_173,_174,_175]];}break;default:var _178=_177;if(56448>_178){return new F(function(){return _16R(_);});}else{if(_178>=56576){return new F(function(){return _16R(_);});}else{var _=writeOffAddr("w8",1,plusAddr(_170,_175),0,_178>>>0&255),_=0;return [0,[0,_16U,_16V,_16W,_16X,_16Y+1|0,_16Z],[0,_170,_171,_172,_173,_174,_175+1|0]];}}}},_179=function(_17a,_17b,_17c,_){var _17d=E(_17b),_17e=E(_17c);return new F(function(){return _16S(_17a,_17d[1],_17d[2],_17d[3],_17d[4],_17d[5],_17d[6],_17e[1],_17e[2],_17e[3],_17e[4],_17e[5],_17e[6],_);});},_17f=function(_17g,_17h,_){return [0,_17h,new T(function(){var _17i=new T(function(){var _17j=B(_dO(_15F,_17h));return [0,_17j[1],_17j[2]];});return B(_165(new T(function(){return E(E(_17i)[1]);}),new T(function(){return B(_5B(_14b,new T(function(){return E(E(_17i)[2]);},1)));}),function(_17k,_17l,_){return new F(function(){return _16I(_17g,_17k,_17l,_);});},_15z));}),new T(function(){return B(_165(_14b,_17h,function(_17k,_17l,_){return new F(function(){return _179(_17g,_17k,_17l,_);});},_15t));})];},_17m=2,_17n=function(_17o,_17p,_17q,_17r,_17s,_17t,_17u,_17v,_17w,_17x,_17y,_17z,_){var _17A=[0,_17o,_17p,_17q,_17r,0,0],_17B=function(_17C,_17D,_){while(1){var _17E=(function(_17F,_17G,_){if(_17F<_17t){if((_17x-_17G|0)>=2){var _17H=readOffAddr("w32",4,_17o,_17F),_17I=_17H,_=0,_17J=_17I;if(_17J>=65536){if((_17x-_17G|0)>=4){var _17K=_17J-65536|0,_=writeOffAddr("w8",1,plusAddr(_17u,_17G),0,((_17K>>18)+216|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_17u,_17G+1|0),0,_17K>>10>>>0&255),_=0,_17L=(_17K>>>0&1023>>>0)>>>0&4294967295,_=writeOffAddr("w8",1,plusAddr(_17u,_17G+2|0),0,((_17L>>8)+220|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_17u,_17G+3|0),0,_17L>>>0&255),_=0,_17M=_17F+1|0,_17N=_17G+4|0;_17C=_17M;_17D=_17N;return null;}else{return [0,_14d,new T(function(){return _17F!=_17t?[0,_17o,_17p,_17q,_17r,_17F,_17t]:E(_17A);}),[0,_17u,_17v,_17w,_17x,_17y,_17G]];}}else{var _17O=function(_17P){if(56320>_17J){var _=writeOffAddr("w8",1,plusAddr(_17u,_17G),0,_17J>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_17u,_17G+1|0),0,_17J>>>0&255),_=0;return new F(function(){return _17B(_17F+1|0,_17G+2|0,_);});}else{if(_17J>57343){var _=writeOffAddr("w8",1,plusAddr(_17u,_17G),0,_17J>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_17u,_17G+1|0),0,_17J>>>0&255),_=0;return new F(function(){return _17B(_17F+1|0,_17G+2|0,_);});}else{return [0,_17m,new T(function(){return _17F!=_17t?[0,_17o,_17p,_17q,_17r,_17F,_17t]:E(_17A);}),[0,_17u,_17v,_17w,_17x,_17y,_17G]];}}};if(55296>_17J){return new F(function(){return _17O(_);});}else{return _17J>56319?B(_17O(_)):[0,_17m,new T(function(){return _17F!=_17t?[0,_17o,_17p,_17q,_17r,_17F,_17t]:E(_17A);}),[0,_17u,_17v,_17w,_17x,_17y,_17G]];}}}else{return [0,_14d,new T(function(){return _17F!=_17t?[0,_17o,_17p,_17q,_17r,_17F,_17t]:E(_17A);}),[0,_17u,_17v,_17w,_17x,_17y,_17G]];}}else{return [0,_14c,new T(function(){return _17F!=_17t?[0,_17o,_17p,_17q,_17r,_17F,_17t]:E(_17A);}),[0,_17u,_17v,_17w,_17x,_17y,_17G]];}})(_17C,_17D,_);if(_17E!=null){return _17E;}}};return new F(function(){return _17B(_17s,_17z,_);});},_17Q=function(_17R,_17S,_17T,_17U,_17V,_17W,_17X,_17Y,_){var _17Z=rMV(_17R),_180=_17Z;if(!E(_180)){if((_17W-_17Y|0)>=2){var _=wMV(_17R,_fI),_=writeOffAddr("w8",1,plusAddr(_17T,_17Y),0,254),_=0,_=writeOffAddr("w8",1,plusAddr(_17T,_17Y+1|0),0,255),_=0,_181=E(_17S);return new F(function(){return _17n(_181[1],_181[2],_181[3],_181[4],_181[5],_181[6],_17T,_17U,_17V,_17W,_17X,_17Y+2|0,_);});}else{return [0,_14d,_17S,[0,_17T,_17U,_17V,_17W,_17X,_17Y]];}}else{var _182=E(_17S);return new F(function(){return _17n(_182[1],_182[2],_182[3],_182[4],_182[5],_182[6],_17T,_17U,_17V,_17W,_17X,_17Y,_);});}},_183=function(_184,_185,_186,_187,_188,_189,_18a,_18b,_18c,_18d,_18e,_18f,_){var _18g=[0,_184,_185,_186,_187,0,0];return new F(function(){return (function(_18h,_18i,_){while(1){var _18j=(function(_18k,_18l,_){if(_18l<_18d){if(_18k<_189){if((_18k+1|0)!=_189){var _18m=readOffAddr("w8",1,plusAddr(_184,_18k),0),_18n=_18m,_=0,_18o=readOffAddr("w8",1,plusAddr(_184,_18k+1|0),0),_18p=_18o,_=0,_18q=(_18n<<8>>>0&65535)+_18p>>>0&65535;if(_18q>=55296){if(_18q<=57343){if((_189-_18k|0)>=4){var _18r=readOffAddr("w8",1,plusAddr(_184,_18k+2|0),0),_18s=_18r,_=0,_18t=readOffAddr("w8",1,plusAddr(_184,_18k+3|0),0),_18u=_18t,_=0;if(_18q<55296){return [0,_17m,new T(function(){return _18k!=_189?[0,_184,_185,_186,_187,_18k,_189]:E(_18g);}),[0,_18a,_18b,_18c,_18d,_18e,_18l]];}else{if(_18q>56319){return [0,_17m,new T(function(){return _18k!=_189?[0,_184,_185,_186,_187,_18k,_189]:E(_18g);}),[0,_18a,_18b,_18c,_18d,_18e,_18l]];}else{var _18v=(_18s<<8>>>0&65535)+_18u>>>0&65535;if(_18v<56320){return [0,_17m,new T(function(){return _18k!=_189?[0,_184,_185,_186,_187,_18k,_189]:E(_18g);}),[0,_18a,_18b,_18c,_18d,_18e,_18l]];}else{if(_18v>57343){return [0,_17m,new T(function(){return _18k!=_189?[0,_184,_185,_186,_187,_18k,_189]:E(_18g);}),[0,_18a,_18b,_18c,_18d,_18e,_18l]];}else{var _=writeOffAddr("w32",4,_18a,_18l,((((_18q&4294967295)-55296|0)<<10)+((_18v&4294967295)-56320|0)|0)+65536|0),_=0,_18w=_18k+4|0,_18x=_18l+1|0;_18h=_18w;_18i=_18x;return null;}}}}}else{return [0,_14c,new T(function(){return _18k!=_189?[0,_184,_185,_186,_187,_18k,_189]:E(_18g);}),[0,_18a,_18b,_18c,_18d,_18e,_18l]];}}else{var _=writeOffAddr("w32",4,_18a,_18l,_18q&4294967295),_=0,_18w=_18k+2|0,_18x=_18l+1|0;_18h=_18w;_18i=_18x;return null;}}else{var _=writeOffAddr("w32",4,_18a,_18l,_18q&4294967295),_=0,_18w=_18k+2|0,_18x=_18l+1|0;_18h=_18w;_18i=_18x;return null;}}else{return [0,_14c,new T(function(){return _18k!=_189?[0,_184,_185,_186,_187,_18k,_189]:E(_18g);}),[0,_18a,_18b,_18c,_18d,_18e,_18l]];}}else{return [0,_14c,new T(function(){return _18k!=_189?[0,_184,_185,_186,_187,_18k,_189]:E(_18g);}),[0,_18a,_18b,_18c,_18d,_18e,_18l]];}}else{return [0,_14d,new T(function(){return _18k!=_189?[0,_184,_185,_186,_187,_18k,_189]:E(_18g);}),[0,_18a,_18b,_18c,_18d,_18e,_18l]];}})(_18h,_18i,_);if(_18j!=null){return _18j;}}})(_188,_18f,_);});},_18y=function(_18z,_18A,_18B,_18C,_18D,_18E,_18F,_18G,_18H,_18I,_18J,_18K,_){var _18L=[0,_18z,_18A,_18B,_18C,0,0];return new F(function(){return (function(_18M,_18N,_){while(1){var _18O=(function(_18P,_18Q,_){if(_18Q<_18I){if(_18P<_18E){if((_18P+1|0)!=_18E){var _18R=readOffAddr("w8",1,plusAddr(_18z,_18P),0),_18S=_18R,_=0,_18T=readOffAddr("w8",1,plusAddr(_18z,_18P+1|0),0),_18U=_18T,_=0,_18V=(_18U<<8>>>0&65535)+_18S>>>0&65535;if(_18V>=55296){if(_18V<=57343){if((_18E-_18P|0)>=4){var _18W=readOffAddr("w8",1,plusAddr(_18z,_18P+2|0),0),_18X=_18W,_=0,_18Y=readOffAddr("w8",1,plusAddr(_18z,_18P+3|0),0),_18Z=_18Y,_=0;if(_18V<55296){return [0,_17m,new T(function(){return _18P!=_18E?[0,_18z,_18A,_18B,_18C,_18P,_18E]:E(_18L);}),[0,_18F,_18G,_18H,_18I,_18J,_18Q]];}else{if(_18V>56319){return [0,_17m,new T(function(){return _18P!=_18E?[0,_18z,_18A,_18B,_18C,_18P,_18E]:E(_18L);}),[0,_18F,_18G,_18H,_18I,_18J,_18Q]];}else{var _190=(_18Z<<8>>>0&65535)+_18X>>>0&65535;if(_190<56320){return [0,_17m,new T(function(){return _18P!=_18E?[0,_18z,_18A,_18B,_18C,_18P,_18E]:E(_18L);}),[0,_18F,_18G,_18H,_18I,_18J,_18Q]];}else{if(_190>57343){return [0,_17m,new T(function(){return _18P!=_18E?[0,_18z,_18A,_18B,_18C,_18P,_18E]:E(_18L);}),[0,_18F,_18G,_18H,_18I,_18J,_18Q]];}else{var _=writeOffAddr("w32",4,_18F,_18Q,((((_18V&4294967295)-55296|0)<<10)+((_190&4294967295)-56320|0)|0)+65536|0),_=0,_191=_18P+4|0,_192=_18Q+1|0;_18M=_191;_18N=_192;return null;}}}}}else{return [0,_14c,new T(function(){return _18P!=_18E?[0,_18z,_18A,_18B,_18C,_18P,_18E]:E(_18L);}),[0,_18F,_18G,_18H,_18I,_18J,_18Q]];}}else{var _=writeOffAddr("w32",4,_18F,_18Q,_18V&4294967295),_=0,_191=_18P+2|0,_192=_18Q+1|0;_18M=_191;_18N=_192;return null;}}else{var _=writeOffAddr("w32",4,_18F,_18Q,_18V&4294967295),_=0,_191=_18P+2|0,_192=_18Q+1|0;_18M=_191;_18N=_192;return null;}}else{return [0,_14c,new T(function(){return _18P!=_18E?[0,_18z,_18A,_18B,_18C,_18P,_18E]:E(_18L);}),[0,_18F,_18G,_18H,_18I,_18J,_18Q]];}}else{return [0,_14c,new T(function(){return _18P!=_18E?[0,_18z,_18A,_18B,_18C,_18P,_18E]:E(_18L);}),[0,_18F,_18G,_18H,_18I,_18J,_18Q]];}}else{return [0,_14d,new T(function(){return _18P!=_18E?[0,_18z,_18A,_18B,_18C,_18P,_18E]:E(_18L);}),[0,_18F,_18G,_18H,_18I,_18J,_18Q]];}})(_18M,_18N,_);if(_18O!=null){return _18O;}}})(_18D,_18K,_);});},_193=function(_194,_195,_){var _196=E(_194),_197=E(_195);return new F(function(){return _183(_196[1],_196[2],_196[3],_196[4],_196[5],_196[6],_197[1],_197[2],_197[3],_197[4],_197[5],_197[6],_);});},_198=[1,_193],_199=function(_19a,_19b,_){var _19c=E(_19a),_19d=E(_19b);return new F(function(){return _18y(_19c[1],_19c[2],_19c[3],_19c[4],_19c[5],_19c[6],_19d[1],_19d[2],_19d[3],_19d[4],_19d[5],_19d[6],_);});},_19e=[1,_199],_19f=function(_19g,_19h,_19i,_19j,_19k,_19l,_19m,_19n,_){var _19o=rMV(_19g),_19p=_19o,_19q=E(_19p);if(!_19q[0]){if((_19m-_19l|0)>=2){var _19r=readOffAddr("w8",1,plusAddr(_19h,_19l),0),_19s=_19r,_=0,_19t=readOffAddr("w8",1,plusAddr(_19h,_19l+1|0),0),_19u=_19t,_=0,_19v=function(_19w){if(E(_19s)==255){if(E(_19u)==254){var _=wMV(_19g,_19e),_19x=E(_19n);return new F(function(){return _18y(_19h,_19i,_19j,_19k,_19l+2|0,_19m,_19x[1],_19x[2],_19x[3],_19x[4],_19x[5],_19x[6],_);});}else{var _=wMV(_19g,_198),_19y=E(_19n);return new F(function(){return _183(_19h,_19i,_19j,_19k,_19l,_19m,_19y[1],_19y[2],_19y[3],_19y[4],_19y[5],_19y[6],_);});}}else{var _=wMV(_19g,_198),_19z=E(_19n);return new F(function(){return _183(_19h,_19i,_19j,_19k,_19l,_19m,_19z[1],_19z[2],_19z[3],_19z[4],_19z[5],_19z[6],_);});}};if(E(_19s)==254){if(E(_19u)==255){var _=wMV(_19g,_198),_19A=E(_19n);return new F(function(){return _183(_19h,_19i,_19j,_19k,_19l+2|0,_19m,_19A[1],_19A[2],_19A[3],_19A[4],_19A[5],_19A[6],_);});}else{return new F(function(){return _19v(_);});}}else{return new F(function(){return _19v(_);});}}else{return [0,_14c,[0,_19h,_19i,_19j,_19k,_19l,_19m],_19n];}}else{return new F(function(){return A(_19q[1],[[0,_19h,_19i,_19j,_19k,_19l,_19m],_19n,_]);});}},_19B=function(_){return _c;},_19C=new T(function(){return B(unCStr("UTF-16"));}),_19D=function(_19E){return [0,_19C,function(_){var _19F=nMV(_5A),_19G=_19F;return [0,function(_19H,_19I,_){var _19J=E(_19H);return new F(function(){return _19f(_19G,_19J[1],_19J[2],_19J[3],_19J[4],_19J[5],_19J[6],_19I,_);});},function(_19K,_19L,_){return new F(function(){return _16I(_19E,_19K,_19L,_);});},_19B,function(_){return new F(function(){return rMV(_19G);});},function(_19M,_){var _=wMV(_19G,_19M);return _c;}];},function(_){var _19N=nMV(_61),_19O=_19N;return [0,function(_19P,_19Q,_){var _19R=E(_19Q);return new F(function(){return _17Q(_19O,_19P,_19R[1],_19R[2],_19R[3],_19R[4],_19R[5],_19R[6],_);});},function(_19K,_19L,_){return new F(function(){return _179(_19E,_19K,_19L,_);});},_19B,function(_){return new F(function(){return rMV(_19O);});},function(_19S,_){var _=wMV(_19O,_19S);return _c;}];}];},_19T=function(_19U,_19V,_){var _19W=E(_19U),_19X=E(_19V);return new F(function(){return _17n(_19W[1],_19W[2],_19W[3],_19W[4],_19W[5],_19W[6],_19X[1],_19X[2],_19X[3],_19X[4],_19X[5],_19X[6],_);});},_19Y=function(_19Z,_){return _c;},_1a0=new T(function(){return B(unCStr("UTF-16BE"));}),_1a1=function(_1a2){return [0,_1a0,function(_){return [0,_193,function(_19K,_19L,_){return new F(function(){return _16I(_1a2,_19K,_19L,_);});},_19B,_19B,_19Y];},function(_){return [0,_19T,function(_19K,_19L,_){return new F(function(){return _179(_1a2,_19K,_19L,_);});},_19B,_19B,_19Y];}];},_1a3=function(_1a4,_1a5,_1a6,_1a7,_1a8,_1a9,_1aa,_1ab,_1ac,_1ad,_1ae,_1af,_){var _1ag=[0,_1a4,_1a5,_1a6,_1a7,0,0],_1ah=function(_1ai,_1aj,_){while(1){var _1ak=(function(_1al,_1am,_){if(_1al<_1a9){if((_1ad-_1am|0)>=2){var _1an=readOffAddr("w32",4,_1a4,_1al),_1ao=_1an,_=0,_1ap=_1ao;if(_1ap>=65536){if((_1ad-_1am|0)>=4){var _1aq=_1ap-65536|0,_=writeOffAddr("w8",1,plusAddr(_1aa,_1am),0,_1aq>>10>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1aa,_1am+1|0),0,((_1aq>>18)+216|0)>>>0&255),_=0,_1ar=(_1aq>>>0&1023>>>0)>>>0&4294967295,_=writeOffAddr("w8",1,plusAddr(_1aa,_1am+2|0),0,_1ar>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1aa,_1am+3|0),0,((_1ar>>8)+220|0)>>>0&255),_=0,_1as=_1al+1|0,_1at=_1am+4|0;_1ai=_1as;_1aj=_1at;return null;}else{return [0,_14d,new T(function(){return _1al!=_1a9?[0,_1a4,_1a5,_1a6,_1a7,_1al,_1a9]:E(_1ag);}),[0,_1aa,_1ab,_1ac,_1ad,_1ae,_1am]];}}else{var _1au=function(_1av){if(56320>_1ap){var _=writeOffAddr("w8",1,plusAddr(_1aa,_1am),0,_1ap>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1aa,_1am+1|0),0,_1ap>>8>>>0&255),_=0;return new F(function(){return _1ah(_1al+1|0,_1am+2|0,_);});}else{if(_1ap>57343){var _=writeOffAddr("w8",1,plusAddr(_1aa,_1am),0,_1ap>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1aa,_1am+1|0),0,_1ap>>8>>>0&255),_=0;return new F(function(){return _1ah(_1al+1|0,_1am+2|0,_);});}else{return [0,_17m,new T(function(){return _1al!=_1a9?[0,_1a4,_1a5,_1a6,_1a7,_1al,_1a9]:E(_1ag);}),[0,_1aa,_1ab,_1ac,_1ad,_1ae,_1am]];}}};if(55296>_1ap){return new F(function(){return _1au(_);});}else{return _1ap>56319?B(_1au(_)):[0,_17m,new T(function(){return _1al!=_1a9?[0,_1a4,_1a5,_1a6,_1a7,_1al,_1a9]:E(_1ag);}),[0,_1aa,_1ab,_1ac,_1ad,_1ae,_1am]];}}}else{return [0,_14d,new T(function(){return _1al!=_1a9?[0,_1a4,_1a5,_1a6,_1a7,_1al,_1a9]:E(_1ag);}),[0,_1aa,_1ab,_1ac,_1ad,_1ae,_1am]];}}else{return [0,_14c,new T(function(){return _1al!=_1a9?[0,_1a4,_1a5,_1a6,_1a7,_1al,_1a9]:E(_1ag);}),[0,_1aa,_1ab,_1ac,_1ad,_1ae,_1am]];}})(_1ai,_1aj,_);if(_1ak!=null){return _1ak;}}};return new F(function(){return _1ah(_1a8,_1af,_);});},_1aw=function(_1ax,_1ay,_){var _1az=E(_1ax),_1aA=E(_1ay);return new F(function(){return _1a3(_1az[1],_1az[2],_1az[3],_1az[4],_1az[5],_1az[6],_1aA[1],_1aA[2],_1aA[3],_1aA[4],_1aA[5],_1aA[6],_);});},_1aB=new T(function(){return B(unCStr("UTF16-LE"));}),_1aC=function(_1aD){return [0,_1aB,function(_){return [0,_199,function(_19K,_19L,_){return new F(function(){return _16I(_1aD,_19K,_19L,_);});},_19B,_19B,_19Y];},function(_){return [0,_1aw,function(_19K,_19L,_){return new F(function(){return _179(_1aD,_19K,_19L,_);});},_19B,_19B,_19Y];}];},_1aE=function(_1aF,_1aG,_1aH,_1aI,_1aJ,_1aK,_1aL,_1aM,_1aN,_1aO,_1aP,_1aQ,_){var _1aR=[0,_1aF,_1aG,_1aH,_1aI,0,0],_1aS=function(_1aT,_1aU,_){if(_1aT<_1aK){if((_1aO-_1aU|0)>=4){var _1aV=readOffAddr("w32",4,_1aF,_1aT),_1aW=_1aV,_=0,_1aX=_1aW,_1aY=function(_1aZ){if(56320>_1aX){var _=writeOffAddr("w8",1,plusAddr(_1aL,_1aU),0,_1aX>>24>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1aL,_1aU+1|0),0,_1aX>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1aL,_1aU+2|0),0,_1aX>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1aL,_1aU+3|0),0,_1aX>>>0&255),_=0;return new F(function(){return _1aS(_1aT+1|0,_1aU+4|0,_);});}else{if(_1aX>57343){var _=writeOffAddr("w8",1,plusAddr(_1aL,_1aU),0,_1aX>>24>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1aL,_1aU+1|0),0,_1aX>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1aL,_1aU+2|0),0,_1aX>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1aL,_1aU+3|0),0,_1aX>>>0&255),_=0;return new F(function(){return _1aS(_1aT+1|0,_1aU+4|0,_);});}else{return [0,_17m,new T(function(){return _1aT!=_1aK?[0,_1aF,_1aG,_1aH,_1aI,_1aT,_1aK]:E(_1aR);}),[0,_1aL,_1aM,_1aN,_1aO,_1aP,_1aU]];}}};if(55296>_1aX){return new F(function(){return _1aY(_);});}else{return _1aX>56319?B(_1aY(_)):[0,_17m,new T(function(){return _1aT!=_1aK?[0,_1aF,_1aG,_1aH,_1aI,_1aT,_1aK]:E(_1aR);}),[0,_1aL,_1aM,_1aN,_1aO,_1aP,_1aU]];}}else{return [0,_14d,new T(function(){return _1aT!=_1aK?[0,_1aF,_1aG,_1aH,_1aI,_1aT,_1aK]:E(_1aR);}),[0,_1aL,_1aM,_1aN,_1aO,_1aP,_1aU]];}}else{return [0,_14c,new T(function(){return _1aT!=_1aK?[0,_1aF,_1aG,_1aH,_1aI,_1aT,_1aK]:E(_1aR);}),[0,_1aL,_1aM,_1aN,_1aO,_1aP,_1aU]];}};return new F(function(){return _1aS(_1aJ,_1aQ,_);});},_1b0=function(_1b1,_1b2,_1b3,_1b4,_1b5,_1b6,_1b7,_1b8,_){var _1b9=rMV(_1b1),_1ba=_1b9;if(!E(_1ba)){if((_1b6-_1b8|0)>=4){var _=wMV(_1b1,_fI),_=writeOffAddr("w8",1,plusAddr(_1b3,_1b8),0,0),_=0,_=writeOffAddr("w8",1,plusAddr(_1b3,_1b8+1|0),0,0),_=0,_=writeOffAddr("w8",1,plusAddr(_1b3,_1b8+2|0),0,254),_=0,_=writeOffAddr("w8",1,plusAddr(_1b3,_1b8+3|0),0,255),_=0,_1bb=E(_1b2);return new F(function(){return _1aE(_1bb[1],_1bb[2],_1bb[3],_1bb[4],_1bb[5],_1bb[6],_1b3,_1b4,_1b5,_1b6,_1b7,_1b8+4|0,_);});}else{return [0,_14d,_1b2,[0,_1b3,_1b4,_1b5,_1b6,_1b7,_1b8]];}}else{var _1bc=E(_1b2);return new F(function(){return _1aE(_1bc[1],_1bc[2],_1bc[3],_1bc[4],_1bc[5],_1bc[6],_1b3,_1b4,_1b5,_1b6,_1b7,_1b8,_);});}},_1bd=function(_1be,_1bf,_1bg,_1bh,_1bi,_1bj,_1bk,_1bl,_1bm,_1bn,_1bo,_1bp,_){var _1bq=[0,_1be,_1bf,_1bg,_1bh,0,0],_1br=function(_1bs,_1bt,_){while(1){var _1bu=(function(_1bv,_1bw,_){if(_1bw<_1bn){if((_1bj-_1bv|0)>=4){var _1bx=readOffAddr("w8",1,plusAddr(_1be,_1bv),0),_1by=_1bx,_=0,_1bz=readOffAddr("w8",1,plusAddr(_1be,_1bv+1|0),0),_1bA=_1bz,_=0,_1bB=readOffAddr("w8",1,plusAddr(_1be,_1bv+2|0),0),_1bC=_1bB,_=0,_1bD=readOffAddr("w8",1,plusAddr(_1be,_1bv+3|0),0),_1bE=_1bD,_=0,_1bF=((((_1by&4294967295)<<24)+((_1bA&4294967295)<<16)|0)+((_1bC&4294967295)<<8)|0)+(_1bE&4294967295)|0,_1bG=_1bF,_1bH=function(_1bI){if(_1bG<=57343){return [0,_17m,new T(function(){return _1bv!=_1bj?[0,_1be,_1bf,_1bg,_1bh,_1bv,_1bj]:E(_1bq);}),[0,_1bk,_1bl,_1bm,_1bn,_1bo,_1bw]];}else{if(_1bG>1114111){return [0,_17m,new T(function(){return _1bv!=_1bj?[0,_1be,_1bf,_1bg,_1bh,_1bv,_1bj]:E(_1bq);}),[0,_1bk,_1bl,_1bm,_1bn,_1bo,_1bw]];}else{var _=writeOffAddr("w32",4,_1bk,_1bw,_1bF),_=0;return new F(function(){return _1br(_1bv+4|0,_1bw+1|0,_);});}}};if(_1bG<0){return new F(function(){return _1bH(_);});}else{if(_1bG>=55296){return new F(function(){return _1bH(_);});}else{var _=writeOffAddr("w32",4,_1bk,_1bw,_1bF),_=0,_1bJ=_1bv+4|0,_1bK=_1bw+1|0;_1bs=_1bJ;_1bt=_1bK;return null;}}}else{return [0,_14c,new T(function(){return _1bv!=_1bj?[0,_1be,_1bf,_1bg,_1bh,_1bv,_1bj]:E(_1bq);}),[0,_1bk,_1bl,_1bm,_1bn,_1bo,_1bw]];}}else{return [0,_14d,new T(function(){return _1bv!=_1bj?[0,_1be,_1bf,_1bg,_1bh,_1bv,_1bj]:E(_1bq);}),[0,_1bk,_1bl,_1bm,_1bn,_1bo,_1bw]];}})(_1bs,_1bt,_);if(_1bu!=null){return _1bu;}}};return new F(function(){return _1br(_1bi,_1bp,_);});},_1bL=function(_1bM,_1bN,_1bO,_1bP,_1bQ,_1bR,_1bS,_1bT,_1bU,_1bV,_1bW,_1bX,_){var _1bY=[0,_1bM,_1bN,_1bO,_1bP,0,0],_1bZ=function(_1c0,_1c1,_){while(1){var _1c2=(function(_1c3,_1c4,_){if(_1c4<_1bV){if((_1bR-_1c3|0)>=4){var _1c5=readOffAddr("w8",1,plusAddr(_1bM,_1c3),0),_1c6=_1c5,_=0,_1c7=readOffAddr("w8",1,plusAddr(_1bM,_1c3+1|0),0),_1c8=_1c7,_=0,_1c9=readOffAddr("w8",1,plusAddr(_1bM,_1c3+2|0),0),_1ca=_1c9,_=0,_1cb=readOffAddr("w8",1,plusAddr(_1bM,_1c3+3|0),0),_1cc=_1cb,_=0,_1cd=((((_1cc&4294967295)<<24)+((_1ca&4294967295)<<16)|0)+((_1c8&4294967295)<<8)|0)+(_1c6&4294967295)|0,_1ce=_1cd,_1cf=function(_1cg){if(_1ce<=57343){return [0,_17m,new T(function(){return _1c3!=_1bR?[0,_1bM,_1bN,_1bO,_1bP,_1c3,_1bR]:E(_1bY);}),[0,_1bS,_1bT,_1bU,_1bV,_1bW,_1c4]];}else{if(_1ce>1114111){return [0,_17m,new T(function(){return _1c3!=_1bR?[0,_1bM,_1bN,_1bO,_1bP,_1c3,_1bR]:E(_1bY);}),[0,_1bS,_1bT,_1bU,_1bV,_1bW,_1c4]];}else{var _=writeOffAddr("w32",4,_1bS,_1c4,_1cd),_=0;return new F(function(){return _1bZ(_1c3+4|0,_1c4+1|0,_);});}}};if(_1ce<0){return new F(function(){return _1cf(_);});}else{if(_1ce>=55296){return new F(function(){return _1cf(_);});}else{var _=writeOffAddr("w32",4,_1bS,_1c4,_1cd),_=0,_1ch=_1c3+4|0,_1ci=_1c4+1|0;_1c0=_1ch;_1c1=_1ci;return null;}}}else{return [0,_14c,new T(function(){return _1c3!=_1bR?[0,_1bM,_1bN,_1bO,_1bP,_1c3,_1bR]:E(_1bY);}),[0,_1bS,_1bT,_1bU,_1bV,_1bW,_1c4]];}}else{return [0,_14d,new T(function(){return _1c3!=_1bR?[0,_1bM,_1bN,_1bO,_1bP,_1c3,_1bR]:E(_1bY);}),[0,_1bS,_1bT,_1bU,_1bV,_1bW,_1c4]];}})(_1c0,_1c1,_);if(_1c2!=null){return _1c2;}}};return new F(function(){return _1bZ(_1bQ,_1bX,_);});},_1cj=function(_1ck,_1cl,_){var _1cm=E(_1ck),_1cn=E(_1cl);return new F(function(){return _1bd(_1cm[1],_1cm[2],_1cm[3],_1cm[4],_1cm[5],_1cm[6],_1cn[1],_1cn[2],_1cn[3],_1cn[4],_1cn[5],_1cn[6],_);});},_1co=[1,_1cj],_1cp=function(_1cq,_1cr,_){var _1cs=E(_1cq),_1ct=E(_1cr);return new F(function(){return _1bL(_1cs[1],_1cs[2],_1cs[3],_1cs[4],_1cs[5],_1cs[6],_1ct[1],_1ct[2],_1ct[3],_1ct[4],_1ct[5],_1ct[6],_);});},_1cu=[1,_1cp],_1cv=function(_1cw,_1cx,_1cy,_1cz,_1cA,_1cB,_1cC,_1cD,_){var _1cE=rMV(_1cw),_1cF=_1cE,_1cG=E(_1cF);if(!_1cG[0]){if((_1cC-_1cB|0)>=4){var _1cH=readOffAddr("w8",1,plusAddr(_1cx,_1cB),0),_1cI=_1cH,_=0,_1cJ=readOffAddr("w8",1,plusAddr(_1cx,_1cB+1|0),0),_1cK=_1cJ,_=0,_1cL=readOffAddr("w8",1,plusAddr(_1cx,_1cB+2|0),0),_1cM=_1cL,_=0,_1cN=readOffAddr("w8",1,plusAddr(_1cx,_1cB+3|0),0),_1cO=_1cN,_=0,_1cP=function(_1cQ){if(E(_1cI)==255){if(E(_1cK)==254){if(!E(_1cM)){if(!E(_1cO)){var _=wMV(_1cw,_1cu),_1cR=E(_1cD);return new F(function(){return _1bL(_1cx,_1cy,_1cz,_1cA,_1cB+4|0,_1cC,_1cR[1],_1cR[2],_1cR[3],_1cR[4],_1cR[5],_1cR[6],_);});}else{var _=wMV(_1cw,_1co),_1cS=E(_1cD);return new F(function(){return _1bd(_1cx,_1cy,_1cz,_1cA,_1cB,_1cC,_1cS[1],_1cS[2],_1cS[3],_1cS[4],_1cS[5],_1cS[6],_);});}}else{var _=wMV(_1cw,_1co),_1cT=E(_1cD);return new F(function(){return _1bd(_1cx,_1cy,_1cz,_1cA,_1cB,_1cC,_1cT[1],_1cT[2],_1cT[3],_1cT[4],_1cT[5],_1cT[6],_);});}}else{var _=wMV(_1cw,_1co),_1cU=E(_1cD);return new F(function(){return _1bd(_1cx,_1cy,_1cz,_1cA,_1cB,_1cC,_1cU[1],_1cU[2],_1cU[3],_1cU[4],_1cU[5],_1cU[6],_);});}}else{var _=wMV(_1cw,_1co),_1cV=E(_1cD);return new F(function(){return _1bd(_1cx,_1cy,_1cz,_1cA,_1cB,_1cC,_1cV[1],_1cV[2],_1cV[3],_1cV[4],_1cV[5],_1cV[6],_);});}};if(!E(_1cI)){if(!E(_1cK)){if(E(_1cM)==254){if(E(_1cO)==255){var _=wMV(_1cw,_1co),_1cW=E(_1cD);return new F(function(){return _1bd(_1cx,_1cy,_1cz,_1cA,_1cB+4|0,_1cC,_1cW[1],_1cW[2],_1cW[3],_1cW[4],_1cW[5],_1cW[6],_);});}else{return new F(function(){return _1cP(_);});}}else{return new F(function(){return _1cP(_);});}}else{return new F(function(){return _1cP(_);});}}else{return new F(function(){return _1cP(_);});}}else{return [0,_14c,[0,_1cx,_1cy,_1cz,_1cA,_1cB,_1cC],_1cD];}}else{return new F(function(){return A(_1cG[1],[[0,_1cx,_1cy,_1cz,_1cA,_1cB,_1cC],_1cD,_]);});}},_1cX=function(_){return _c;},_1cY=new T(function(){return B(unCStr("UTF-32"));}),_1cZ=function(_1d0){return [0,_1cY,function(_){var _1d1=nMV(_5A),_1d2=_1d1;return [0,function(_1d3,_1d4,_){var _1d5=E(_1d3);return new F(function(){return _1cv(_1d2,_1d5[1],_1d5[2],_1d5[3],_1d5[4],_1d5[5],_1d5[6],_1d4,_);});},function(_1d6,_1d7,_){return new F(function(){return _16I(_1d0,_1d6,_1d7,_);});},_1cX,function(_){return new F(function(){return rMV(_1d2);});},function(_1d8,_){var _=wMV(_1d2,_1d8);return _c;}];},function(_){var _1d9=nMV(_61),_1da=_1d9;return [0,function(_1db,_1dc,_){var _1dd=E(_1dc);return new F(function(){return _1b0(_1da,_1db,_1dd[1],_1dd[2],_1dd[3],_1dd[4],_1dd[5],_1dd[6],_);});},function(_1d6,_1d7,_){return new F(function(){return _179(_1d0,_1d6,_1d7,_);});},_1cX,function(_){return new F(function(){return rMV(_1da);});},function(_1de,_){var _=wMV(_1da,_1de);return _c;}];}];},_1df=function(_1dg,_1dh,_){var _1di=E(_1dg),_1dj=E(_1dh);return new F(function(){return _1aE(_1di[1],_1di[2],_1di[3],_1di[4],_1di[5],_1di[6],_1dj[1],_1dj[2],_1dj[3],_1dj[4],_1dj[5],_1dj[6],_);});},_1dk=function(_1dl,_){return _c;},_1dm=new T(function(){return B(unCStr("UTF-32BE"));}),_1dn=function(_1do){return [0,_1dm,function(_){return [0,_1cj,function(_1d6,_1d7,_){return new F(function(){return _16I(_1do,_1d6,_1d7,_);});},_1cX,_1cX,_1dk];},function(_){return [0,_1df,function(_1d6,_1d7,_){return new F(function(){return _179(_1do,_1d6,_1d7,_);});},_1cX,_1cX,_1dk];}];},_1dp=function(_1dq,_1dr,_1ds,_1dt,_1du,_1dv,_1dw,_1dx,_1dy,_1dz,_1dA,_1dB,_){var _1dC=[0,_1dq,_1dr,_1ds,_1dt,0,0],_1dD=function(_1dE,_1dF,_){if(_1dE<_1dv){if((_1dz-_1dF|0)>=4){var _1dG=readOffAddr("w32",4,_1dq,_1dE),_1dH=_1dG,_=0,_1dI=_1dH,_1dJ=function(_1dK){if(56320>_1dI){var _=writeOffAddr("w8",1,plusAddr(_1dw,_1dF),0,_1dI>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1dw,_1dF+1|0),0,_1dI>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1dw,_1dF+2|0),0,_1dI>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1dw,_1dF+3|0),0,_1dI>>24>>>0&255),_=0;return new F(function(){return _1dD(_1dE+1|0,_1dF+4|0,_);});}else{if(_1dI>57343){var _=writeOffAddr("w8",1,plusAddr(_1dw,_1dF),0,_1dI>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1dw,_1dF+1|0),0,_1dI>>8>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1dw,_1dF+2|0),0,_1dI>>16>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1dw,_1dF+3|0),0,_1dI>>24>>>0&255),_=0;return new F(function(){return _1dD(_1dE+1|0,_1dF+4|0,_);});}else{return [0,_17m,new T(function(){return _1dE!=_1dv?[0,_1dq,_1dr,_1ds,_1dt,_1dE,_1dv]:E(_1dC);}),[0,_1dw,_1dx,_1dy,_1dz,_1dA,_1dF]];}}};if(55296>_1dI){return new F(function(){return _1dJ(_);});}else{return _1dI>56319?B(_1dJ(_)):[0,_17m,new T(function(){return _1dE!=_1dv?[0,_1dq,_1dr,_1ds,_1dt,_1dE,_1dv]:E(_1dC);}),[0,_1dw,_1dx,_1dy,_1dz,_1dA,_1dF]];}}else{return [0,_14d,new T(function(){return _1dE!=_1dv?[0,_1dq,_1dr,_1ds,_1dt,_1dE,_1dv]:E(_1dC);}),[0,_1dw,_1dx,_1dy,_1dz,_1dA,_1dF]];}}else{return [0,_14c,new T(function(){return _1dE!=_1dv?[0,_1dq,_1dr,_1ds,_1dt,_1dE,_1dv]:E(_1dC);}),[0,_1dw,_1dx,_1dy,_1dz,_1dA,_1dF]];}};return new F(function(){return _1dD(_1du,_1dB,_);});},_1dL=function(_1dM,_1dN,_){var _1dO=E(_1dM),_1dP=E(_1dN);return new F(function(){return _1dp(_1dO[1],_1dO[2],_1dO[3],_1dO[4],_1dO[5],_1dO[6],_1dP[1],_1dP[2],_1dP[3],_1dP[4],_1dP[5],_1dP[6],_);});},_1dQ=new T(function(){return B(unCStr("UTF-32LE"));}),_1dR=function(_1dS){return [0,_1dQ,function(_){return [0,_1cp,function(_1d6,_1d7,_){return new F(function(){return _16I(_1dS,_1d6,_1d7,_);});},_1cX,_1cX,_1dk];},function(_){return [0,_1dL,function(_1d6,_1d7,_){return new F(function(){return _179(_1dS,_1d6,_1d7,_);});},_1cX,_1cX,_1dk];}];},_1dT=function(_1dU,_1dV,_1dW,_1dX,_1dY,_1dZ,_1e0,_1e1,_1e2,_1e3,_1e4,_1e5,_){var _1e6=[0,_1dU,_1dV,_1dW,_1dX,0,0],_1e7=function(_1e8,_1e9,_){while(1){var _1ea=(function(_1eb,_1ec,_){if(_1ec<_1e3){if(_1eb<_1dZ){var _1ed=readOffAddr("w32",4,_1dU,_1eb),_1ee=_1ed,_=0,_1ef=_1ee;if(_1ef>127){if(_1ef>2047){if(_1ef>65535){if((_1e3-_1ec|0)>=4){var _=writeOffAddr("w8",1,plusAddr(_1e0,_1ec),0,((_1ef>>18)+240|0)>>>0&255),_=0,_1eg=63>>>0,_=writeOffAddr("w8",1,plusAddr(_1e0,_1ec+1|0),0,(((_1ef>>12>>>0&_1eg)>>>0&4294967295)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1e0,_1ec+2|0),0,(((_1ef>>6>>>0&_1eg)>>>0&4294967295)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1e0,_1ec+3|0),0,(((_1ef>>>0&_1eg)>>>0&4294967295)+128|0)>>>0&255),_=0,_1eh=_1eb+1|0,_1ei=_1ec+4|0;_1e8=_1eh;_1e9=_1ei;return null;}else{return [0,_14d,new T(function(){return _1eb!=_1dZ?[0,_1dU,_1dV,_1dW,_1dX,_1eb,_1dZ]:E(_1e6);}),[0,_1e0,_1e1,_1e2,_1e3,_1e4,_1ec]];}}else{var _1ej=function(_1ek){var _1el=function(_1em){if((_1e3-_1ec|0)>=3){var _=writeOffAddr("w8",1,plusAddr(_1e0,_1ec),0,((_1ef>>12)+224|0)>>>0&255),_=0,_1en=63>>>0,_=writeOffAddr("w8",1,plusAddr(_1e0,_1ec+1|0),0,(((_1ef>>6>>>0&_1en)>>>0&4294967295)+128|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1e0,_1ec+2|0),0,(((_1ef>>>0&_1en)>>>0&4294967295)+128|0)>>>0&255),_=0;return new F(function(){return _1e7(_1eb+1|0,_1ec+3|0,_);});}else{return [0,_14d,new T(function(){return _1eb!=_1dZ?[0,_1dU,_1dV,_1dW,_1dX,_1eb,_1dZ]:E(_1e6);}),[0,_1e0,_1e1,_1e2,_1e3,_1e4,_1ec]];}};if(56320>_1ef){return new F(function(){return _1el(_);});}else{return _1ef>57343?B(_1el(_)):[0,_17m,new T(function(){return _1eb!=_1dZ?[0,_1dU,_1dV,_1dW,_1dX,_1eb,_1dZ]:E(_1e6);}),[0,_1e0,_1e1,_1e2,_1e3,_1e4,_1ec]];}};if(55296>_1ef){return new F(function(){return _1ej(_);});}else{return _1ef>56319?B(_1ej(_)):[0,_17m,new T(function(){return _1eb!=_1dZ?[0,_1dU,_1dV,_1dW,_1dX,_1eb,_1dZ]:E(_1e6);}),[0,_1e0,_1e1,_1e2,_1e3,_1e4,_1ec]];}}}else{if((_1e3-_1ec|0)>=2){var _=writeOffAddr("w8",1,plusAddr(_1e0,_1ec),0,((_1ef>>6)+192|0)>>>0&255),_=0,_=writeOffAddr("w8",1,plusAddr(_1e0,_1ec+1|0),0,(((_1ef>>>0&63>>>0)>>>0&4294967295)+128|0)>>>0&255),_=0,_1eh=_1eb+1|0,_1ei=_1ec+2|0;_1e8=_1eh;_1e9=_1ei;return null;}else{return [0,_14d,new T(function(){return _1eb!=_1dZ?[0,_1dU,_1dV,_1dW,_1dX,_1eb,_1dZ]:E(_1e6);}),[0,_1e0,_1e1,_1e2,_1e3,_1e4,_1ec]];}}}else{var _=writeOffAddr("w8",1,plusAddr(_1e0,_1ec),0,_1ef>>>0&255),_=0,_1eh=_1eb+1|0,_1ei=_1ec+1|0;_1e8=_1eh;_1e9=_1ei;return null;}}else{return [0,_14c,new T(function(){return _1eb!=_1dZ?[0,_1dU,_1dV,_1dW,_1dX,_1eb,_1dZ]:E(_1e6);}),[0,_1e0,_1e1,_1e2,_1e3,_1e4,_1ec]];}}else{return [0,_14d,new T(function(){return _1eb!=_1dZ?[0,_1dU,_1dV,_1dW,_1dX,_1eb,_1dZ]:E(_1e6);}),[0,_1e0,_1e1,_1e2,_1e3,_1e4,_1ec]];}})(_1e8,_1e9,_);if(_1ea!=null){return _1ea;}}};return new F(function(){return _1e7(_1dY,_1e5,_);});},_1eo=function(_1ep,_1eq,_){var _1er=E(_1ep),_1es=E(_1eq);return new F(function(){return _1dT(_1er[1],_1er[2],_1er[3],_1er[4],_1er[5],_1er[6],_1es[1],_1es[2],_1es[3],_1es[4],_1es[5],_1es[6],_);});},_1et=function(_1eu,_){return _c;},_1ev=function(_){return _c;},_1ew=function(_1ex,_1ey,_1ez,_1eA,_1eB,_1eC,_1eD,_1eE,_1eF,_1eG,_1eH,_1eI,_){var _1eJ=[0,_1ex,_1ey,_1ez,_1eA,0,0],_1eK=function(_1eL,_1eM,_){while(1){var _1eN=(function(_1eO,_1eP,_){if(_1eP<_1eG){if(_1eO<_1eC){var _1eQ=readOffAddr("w8",1,plusAddr(_1ex,_1eO),0),_1eR=_1eQ,_=0;if(_1eR>127){var _1eS=function(_1eT){var _1eU=function(_1eV){if(_1eR<240){return [0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];}else{switch(_1eC-_1eO|0){case 1:return [0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];case 2:var _1eW=readOffAddr("w8",1,plusAddr(_1ex,_1eO+1|0),0),_1eX=_1eW,_=0,_1eY=function(_1eZ){var _1f0=function(_1f1){return E(_1eR)==244?_1eX<128?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1eX>143?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:[0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];};if(_1eR<241){return new F(function(){return _1f0(_);});}else{if(_1eR>243){return new F(function(){return _1f0(_);});}else{if(_1eX<128){return new F(function(){return _1f0(_);});}else{return _1eX>191?B(_1f0(_)):[0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];}}}};if(E(_1eR)==240){if(_1eX<144){return new F(function(){return _1eY(_);});}else{return _1eX>191?B(_1eY(_)):[0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];}}else{return new F(function(){return _1eY(_);});}break;case 3:var _1f2=readOffAddr("w8",1,plusAddr(_1ex,_1eO+1|0),0),_1f3=_1f2,_=0,_1f4=readOffAddr("w8",1,plusAddr(_1ex,_1eO+2|0),0),_1f5=_1f4,_=0,_1f6=function(_1f7){var _1f8=function(_1f9){return E(_1eR)==244?_1f3<128?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1f3>143?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1f5<128?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1f5>191?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:[0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];};if(_1eR<241){return new F(function(){return _1f8(_);});}else{if(_1eR>243){return new F(function(){return _1f8(_);});}else{if(_1f3<128){return new F(function(){return _1f8(_);});}else{if(_1f3>191){return new F(function(){return _1f8(_);});}else{if(_1f5<128){return new F(function(){return _1f8(_);});}else{return _1f5>191?B(_1f8(_)):[0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];}}}}}};if(E(_1eR)==240){if(_1f3<144){return new F(function(){return _1f6(_);});}else{if(_1f3>191){return new F(function(){return _1f6(_);});}else{if(_1f5<128){return new F(function(){return _1f6(_);});}else{return _1f5>191?B(_1f6(_)):[0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];}}}}else{return new F(function(){return _1f6(_);});}break;default:var _1fa=readOffAddr("w8",1,plusAddr(_1ex,_1eO+1|0),0),_1fb=_1fa,_=0,_1fc=readOffAddr("w8",1,plusAddr(_1ex,_1eO+2|0),0),_1fd=_1fc,_=0,_1fe=readOffAddr("w8",1,plusAddr(_1ex,_1eO+3|0),0),_1ff=_1fe,_=0,_1fg=function(_1fh){var _=writeOffAddr("w32",4,_1eD,_1eP,(((((_1eR&4294967295)-240|0)<<18)+(((_1fb&4294967295)-128|0)<<12)|0)+(((_1fd&4294967295)-128|0)<<6)|0)+((_1ff&4294967295)-128|0)|0),_=0;return new F(function(){return _1eK(_1eO+4|0,_1eP+1|0,_);});},_1fi=function(_1fj){var _1fk=function(_1fl){return E(_1eR)==244?_1fb<128?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1fb>143?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1fd<128?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1fd>191?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1ff<128?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1ff>191?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:B(_1fg(_)):[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];};if(_1eR<241){return new F(function(){return _1fk(_);});}else{if(_1eR>243){return new F(function(){return _1fk(_);});}else{if(_1fb<128){return new F(function(){return _1fk(_);});}else{if(_1fb>191){return new F(function(){return _1fk(_);});}else{if(_1fd<128){return new F(function(){return _1fk(_);});}else{if(_1fd>191){return new F(function(){return _1fk(_);});}else{if(_1ff<128){return new F(function(){return _1fk(_);});}else{return _1ff>191?B(_1fk(_)):B(_1fg(_));}}}}}}}};if(E(_1eR)==240){if(_1fb<144){return new F(function(){return _1fi(_);});}else{if(_1fb>191){return new F(function(){return _1fi(_);});}else{if(_1fd<128){return new F(function(){return _1fi(_);});}else{if(_1fd>191){return new F(function(){return _1fi(_);});}else{if(_1ff<128){return new F(function(){return _1fi(_);});}else{return _1ff>191?B(_1fi(_)):B(_1fg(_));}}}}}}else{return new F(function(){return _1fi(_);});}}}};if(_1eR<224){return new F(function(){return _1eU(_);});}else{if(_1eR>239){return new F(function(){return _1eU(_);});}else{switch(_1eC-_1eO|0){case 1:return [0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];case 2:var _1fm=readOffAddr("w8",1,plusAddr(_1ex,_1eO+1|0),0),_1fn=_1fm,_=0,_1fo=function(_1fp){var _1fq=function(_1fr){var _1fs=function(_1ft){return _1eR<238?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1fn<128?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1fn>191?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:[0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];};if(E(_1eR)==237){if(_1fn<128){return new F(function(){return _1fs(_);});}else{return _1fn>159?B(_1fs(_)):[0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];}}else{return new F(function(){return _1fs(_);});}};if(_1eR<225){return new F(function(){return _1fq(_);});}else{if(_1eR>236){return new F(function(){return _1fq(_);});}else{if(_1fn<128){return new F(function(){return _1fq(_);});}else{return _1fn>191?B(_1fq(_)):[0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];}}}};if(E(_1eR)==224){if(_1fn<160){return new F(function(){return _1fo(_);});}else{return _1fn>191?B(_1fo(_)):[0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];}}else{return new F(function(){return _1fo(_);});}break;default:var _1fu=readOffAddr("w8",1,plusAddr(_1ex,_1eO+1|0),0),_1fv=_1fu,_=0,_1fw=readOffAddr("w8",1,plusAddr(_1ex,_1eO+2|0),0),_1fx=_1fw,_=0,_1fy=function(_1fz){var _=writeOffAddr("w32",4,_1eD,_1eP,((((_1eR&4294967295)-224|0)<<12)+(((_1fv&4294967295)-128|0)<<6)|0)+((_1fx&4294967295)-128|0)|0),_=0;return new F(function(){return _1eK(_1eO+3|0,_1eP+1|0,_);});},_1fA=function(_1fB){var _1fC=function(_1fD){var _1fE=function(_1fF){return _1eR<238?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1fv<128?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1fv>191?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1fx<128?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:_1fx>191?[0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]]:B(_1fy(_));};if(E(_1eR)==237){if(_1fv<128){return new F(function(){return _1fE(_);});}else{if(_1fv>159){return new F(function(){return _1fE(_);});}else{if(_1fx<128){return new F(function(){return _1fE(_);});}else{return _1fx>191?B(_1fE(_)):B(_1fy(_));}}}}else{return new F(function(){return _1fE(_);});}};if(_1eR<225){return new F(function(){return _1fC(_);});}else{if(_1eR>236){return new F(function(){return _1fC(_);});}else{if(_1fv<128){return new F(function(){return _1fC(_);});}else{if(_1fv>191){return new F(function(){return _1fC(_);});}else{if(_1fx<128){return new F(function(){return _1fC(_);});}else{return _1fx>191?B(_1fC(_)):B(_1fy(_));}}}}}};if(E(_1eR)==224){if(_1fv<160){return new F(function(){return _1fA(_);});}else{if(_1fv>191){return new F(function(){return _1fA(_);});}else{if(_1fx<128){return new F(function(){return _1fA(_);});}else{return _1fx>191?B(_1fA(_)):B(_1fy(_));}}}}else{return new F(function(){return _1fA(_);});}}}}};if(_1eR<192){return new F(function(){return _1eS(_);});}else{if(_1eR>223){return new F(function(){return _1eS(_);});}else{if((_1eC-_1eO|0)>=2){var _1fG=readOffAddr("w8",1,plusAddr(_1ex,_1eO+1|0),0),_1fH=_1fG,_=0;if(_1fH>=128){if(_1fH<192){var _=writeOffAddr("w32",4,_1eD,_1eP,(((_1eR&4294967295)-192|0)<<6)+((_1fH&4294967295)-128|0)|0),_=0,_1fI=_1eO+2|0,_1fJ=_1eP+1|0;_1eL=_1fI;_1eM=_1fJ;return null;}else{return [0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];}}else{return [0,_17m,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];}}else{return [0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];}}}}else{var _=writeOffAddr("w32",4,_1eD,_1eP,_1eR&4294967295),_=0,_1fI=_1eO+1|0,_1fJ=_1eP+1|0;_1eL=_1fI;_1eM=_1fJ;return null;}}else{return [0,_14c,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];}}else{return [0,_14d,new T(function(){return _1eO!=_1eC?[0,_1ex,_1ey,_1ez,_1eA,_1eO,_1eC]:E(_1eJ);}),[0,_1eD,_1eE,_1eF,_1eG,_1eH,_1eP]];}})(_1eL,_1eM,_);if(_1eN!=null){return _1eN;}}};return new F(function(){return _1eK(_1eB,_1eI,_);});},_1fK=function(_1fL,_1fM,_){var _1fN=E(_1fL),_1fO=E(_1fM);return new F(function(){return _1ew(_1fN[1],_1fN[2],_1fN[3],_1fN[4],_1fN[5],_1fN[6],_1fO[1],_1fO[2],_1fO[3],_1fO[4],_1fO[5],_1fO[6],_);});},_1fP=new T(function(){return B(unCStr("UTF-8"));}),_1fQ=function(_1fR){return [0,_1fP,function(_){return [0,_1fK,function(_1fS,_1fT,_){return new F(function(){return _16I(_1fR,_1fS,_1fT,_);});},_1ev,_1ev,_1et];},function(_){return [0,_1eo,function(_1fS,_1fT,_){return new F(function(){return _179(_1fR,_1fS,_1fT,_);});},_1ev,_1ev,_1et];}];},_1fU=function(_1fV,_1fW,_){var _1fX=B(_144(_1fW));return !B(_h5(_1fX,_13V))?!B(_h5(_1fX,_13U))?!B(_h5(_1fX,_13T))?!B(_h5(_1fX,_13Z))?!B(_h5(_1fX,_13Y))?!B(_h5(_1fX,_13X))?!B(_h5(_1fX,_13W))?B(_17f(_1fV,_1fW,_)):new T(function(){return B(_1fQ(_1fV));}):new T(function(){return B(_1dR(_1fV));}):new T(function(){return B(_1dn(_1fV));}):new T(function(){return B(_1cZ(_1fV));}):new T(function(){return B(_1aC(_1fV));}):new T(function(){return B(_1a1(_1fV));}):new T(function(){return B(_19D(_1fV));});},_1fY=function(_1fZ,_){var _1g0=B((function(_1g1,_){while(1){var _1g2=readOffAddr("i8",1,_1fZ,_1g1),_1g3=_1g2;if(!E(_1g3)){return [0,_1g1];}else{var _1g4=_1g1+1|0;_1g1=_1g4;continue;}}})(0,_)),_1g5=_1g0,_1g6=E(_1g5)[1];if(_1g6>0){return new F(function(){return (function(_1g7,_1g8,_){while(1){var _1g9=readOffAddr("i8",1,_1fZ,_1g8),_1ga=_1g9;if(_1g8>0){var _1gb=[1,[0,_1ga>>>0&255&4294967295],_1g7],_1gc=_1g8-1|0;_1g7=_1gb;_1g8=_1gc;continue;}else{return [1,[0,_1ga>>>0&255&4294967295],_1g7];}}})(_1g,_1g6-1|0,_);});}else{return _1g;}},_1gd=function(_){var _=0,_1ge=localeEncoding(),_1gf=_1ge;return new F(function(){return _1fY(_1gf,_);});},_1gg=new T(function(){return B(_6(_1gd));}),_1gh=function(_){var _=0;return new F(function(){return _1fU(_13S,_1gg,_);});},_1gi=new T(function(){return B(_6(_1gh));}),_1gj=function(_){var _=0,_1gk=nMV(_1gi),_1gl=_1gk;return [0,function(_){return new F(function(){return rMV(_1gl);});},function(_1gm,_){var _=wMV(_1gl,_1gm);return _c;}];},_1gn=new T(function(){return B(_6(_1gj));}),_15r=function(_1go,_1gp,_1gq,_1gr){return new F(function(){return _6(function(_){var _=0,_1gs=strerror(_1gp),_1gt=_1gs,_1gu=B(A(E(_1gn)[1],[_])),_1gv=_1gu,_1gw=B(_137(_1gv,_1gt,_)),_1gx=_1gw;return [0,_1gq,new T(function(){switch(E(_1gp)){case 1:var _1gy=6;break;case 2:var _1gy=1;break;case 3:var _1gy=1;break;case 4:var _1gy=18;break;case 5:var _1gy=14;break;case 6:var _1gy=1;break;case 7:var _1gy=3;break;case 8:var _1gy=12;break;case 9:var _1gy=12;break;case 10:var _1gy=1;break;case 11:var _1gy=3;break;case 12:var _1gy=3;break;case 13:var _1gy=6;break;case 15:var _1gy=12;break;case 16:var _1gy=2;break;case 17:var _1gy=0;break;case 18:var _1gy=15;break;case 19:var _1gy=15;break;case 20:var _1gy=13;break;case 21:var _1gy=13;break;case 22:var _1gy=12;break;case 23:var _1gy=3;break;case 24:var _1gy=3;break;case 25:var _1gy=5;break;case 26:var _1gy=2;break;case 27:var _1gy=6;break;case 28:var _1gy=3;break;case 29:var _1gy=15;break;case 30:var _1gy=6;break;case 31:var _1gy=3;break;case 32:var _1gy=17;break;case 33:var _1gy=12;break;case 34:var _1gy=15;break;case 35:var _1gy=2;break;case 36:var _1gy=12;break;case 37:var _1gy=3;break;case 38:var _1gy=15;break;case 39:var _1gy=8;break;case 40:var _1gy=12;break;case 42:var _1gy=1;break;case 43:var _1gy=17;break;case 60:var _1gy=12;break;case 61:var _1gy=1;break;case 62:var _1gy=16;break;case 63:var _1gy=3;break;case 64:var _1gy=1;break;case 66:var _1gy=5;break;case 67:var _1gy=17;break;case 69:var _1gy=8;break;case 70:var _1gy=17;break;case 71:var _1gy=10;break;case 72:var _1gy=15;break;case 74:var _1gy=13;break;case 78:var _1gy=17;break;case 84:var _1gy=12;break;case 87:var _1gy=3;break;case 88:var _1gy=12;break;case 89:var _1gy=12;break;case 90:var _1gy=3;break;case 91:var _1gy=10;break;case 92:var _1gy=15;break;case 93:var _1gy=10;break;case 94:var _1gy=15;break;case 95:var _1gy=15;break;case 96:var _1gy=15;break;case 97:var _1gy=15;break;case 98:var _1gy=2;break;case 99:var _1gy=15;break;case 100:var _1gy=17;break;case 101:var _1gy=1;break;case 102:var _1gy=17;break;case 104:var _1gy=17;break;case 105:var _1gy=3;break;case 106:var _1gy=0;break;case 107:var _1gy=12;break;case 108:var _1gy=5;break;case 109:var _1gy=3;break;case 110:var _1gy=16;break;case 111:var _1gy=1;break;case 112:var _1gy=1;break;case 113:var _1gy=1;break;case 114:var _1gy=0;break;case 115:var _1gy=0;break;case 116:var _1gy=17;break;case 122:var _1gy=6;break;default:var _1gy=11;}return _1gy;}),_1go,_1gx,[1,[0,_1gp]],_1gr];});});},_1gz=new T(function(){return B(unCStr("gettimeofday"));}),_1gA=function(_){var _1gB=newByteArr(8),_1gC=_1gB,_1gD=_1gC,_1gE=_1gD,_1gF=_1gE,_=writeOffAddr("i32",4,_1gF,0,0),_=writeOffAddr("i32",4,_1gF,1,0),_1gG=gettimeofday(_1gF,0),_1gH=_1gG;if(E(_1gH)==(-1)){var _1gI=__hscore_get_errno(),_1gJ=_1gI;return new F(function(){return _14i(B(_15r(_1gz,_1gJ,_5A,_5A)),_);});}else{var _1gK=readOffAddr("i32",4,_1gF,0),_1gL=_1gK,_1gM=readOffAddr("i32",4,_1gF,1),_1gN=_1gM,_=0;return [0,[0,_1gL],[0,_1gN]];}},_1gO=[1,I_fromBits([2808348672,232830643])],_1gP=function(_){var _1gQ=B(_1gA(_)),_1gR=_1gQ;return new T(function(){var _1gS=E(_1gR);if(!B(_HL(_1gO,_11c))){var _1gT=B(_pT(B(_qb(B(_q9(E(_1gS[1])[1])),_YR)),B(_ZW(B(_qb(B(_qb(B(_q9(E(_1gS[2])[1])),_YR)),_YR)),_1gO))));}else{var _1gT=E(_IO);}var _1gU=_1gT,_1gV=_1gU;return _1gV;});},_1gW=[0,12345],_1gX=function(_){var _=0,_1gY=B(_1gP(_)),_1gZ=_1gY,_1h0=B(_YS(E(B(_11D(_1gZ))[2]),_Jq,_YR,_Jq)),_1h1=_1h0[2];if(!B(_HL(_1h1,_Ya))){var _1h2=B(_10q(_1h0[1],_1h1)),_1h3=nMV(new T(function(){var _1h4=B(_Zc(B(_ro(B(_pT(B(_pT(B(_pT(B(_qb(_1h2[1],_1gW)),_1h2[2])),_11J)),_Ya))))));return [0,_1h4[1],_1h4[2]];})),_1h5=_1h3;return [0,_1h5];}else{return E(_IO);}},_1h6=new T(function(){return B(_6(_1gX));}),_1h7=function(_){var _1h8=mMV(E(_1h6)[1],_YL),_1h9=_1h8,_1ha=E(_1h9);return E(_1h8);},_1hb=[0,2],_1hc=new T(function(){return B(_I1(_XK,_1hb));}),_1hd=new T(function(){return B(_I1(_1hb,_XK));}),_1he=function(_1hf,_1hg,_1hh){while(1){var _1hi=(function(_1hj,_1hk,_1hl){if(_1hj<=_1hk){var _1hm=new T(function(){var _1hn=B(_YI(_1hl));return [0,_1hn[1],_1hn[2]];});return [0,new T(function(){var _1ho=E(_1hc)[1];return [0,E(_1hd)[1]*(_1ho*_1hj+E(E(_1hm)[1])[1]*(_1ho*_1hk-_1ho*_1hj))];}),new T(function(){return E(E(_1hm)[2]);})];}else{var _1hp=_1hk,_1hq=_1hj,_1hr=_1hl;_1hf=_1hp;_1hg=_1hq;_1hh=_1hr;return null;}})(_1hf,_1hg,_1hh);if(_1hi!=null){return _1hi;}}},_1hs=function(_1ht,_1hu){var _1hv=E(_1ht),_1hw=E(_1hv[1])[1],_1hx=E(_1hv[2])[1];if(_1hw<=_1hx){var _1hy=new T(function(){var _1hz=B(_YI(_1hu));return [0,_1hz[1],_1hz[2]];});return [0,new T(function(){var _1hA=E(_1hc)[1];return [0,E(_1hd)[1]*(_1hA*_1hw+E(E(_1hy)[1])[1]*(_1hA*_1hx-_1hA*_1hw))];}),new T(function(){return E(E(_1hy)[2]);})];}else{return new F(function(){return _1he(_1hx,_1hw,_1hu);});}},_1hB=function(_1hC,_){var _1hD=mMV(E(_1h6)[1],function(_1hE){var _1hF=new T(function(){var _1hG=B(_1hs(_1hC,_1hE));return [0,_1hG[2],_1hG[1]];}),_1hH=new T(function(){return E(E(_1hF)[1]);});return [0,_1hH,new T(function(){var _1hI=E(_1hH);return E(E(_1hF)[2]);})];}),_1hJ=_1hD,_1hK=E(_1hJ);return E(_1hD);},_1hL=function(_1hM){return E(E(_1hM)[2]);},_1hN=function(_1hO){return E(E(_1hO)[1]);},_1hP=function(_1hQ,_1hR,_1hS,_1hT,_1hU){while(1){var _1hV=(function(_1hW,_1hX,_1hY,_1hZ,_1i0){if(!B(_Kw(_1hY,_1hZ))){var _1i1=B(_pT(B(_Jv(_1hZ,_1hY)),_XK)),_1i2=new T(function(){return B(A(_1hL,[_1hW,_1i0]));}),_1i3=new T(function(){return E(E(_1i2)[1]);}),_1i4=new T(function(){return B(_pT(B(_Jv(B(_q9(E(E(_1i2)[2])[1])),B(_q9(E(_1i3)[1])))),_XK));}),_1i5=B((function(_1i6,_1i7,_1i8){while(1){if(!B(_K9(_1i6,B(_qb(_1i1,_Yb))))){var _1i9=B(A(new T(function(){return B(_1hN(_1hW));}),[_1i8])),_1ia=B(_qb(_1i6,_1i4)),_1ib=B(_pT(B(_qb(_1i7,_1i4)),B(_Jv(B(_q9(E(_1i9[1])[1])),new T(function(){return B(_q9(E(_1i3)[1]));})))));_1i8=_1i9[2];_1i6=_1ia;_1i7=_1ib;continue;}else{return [0,_1i7,_1i8];}}})(_XK,_Ya,_1i0));return [0,new T(function(){return B(A(_Y8,[_1hX,new T(function(){if(!B(_HL(_1i1,_Ya))){var _1ic=B(_pT(_1hY,B(_Yc(_1i5[1],_1i1))));}else{var _1ic=E(_IO);}return _1ic;})]));}),_1i5[2]];}else{var _1id=_1hW,_1ie=_1hX,_1if=_1hZ,_1ig=_1hY,_1ih=_1i0;_1hQ=_1id;_1hR=_1ie;_1hS=_1if;_1hT=_1ig;_1hU=_1ih;return null;}})(_1hQ,_1hR,_1hS,_1hT,_1hU);if(_1hV!=null){return _1hV;}}},_1ii=function(_1ij,_1ik){var _1il=B(_1hP(_1ij,_W7,B(_Wc(E(_W9)[1])),_W8,_1ik));return [0,new T(function(){return [0,B(_Uk(B(_Wc(B(_Xz(E(_Xw)[1],E(_1il[1])[1]))))))/E(_Xv)[1]];}),_1il[2]];},_1im=function(_1in,_1io){var _1ip=B(_1ii(_1in,_1io));return [0,_1ip[1],_1ip[2]];},_1iq=function(_1ir,_1is,_1it){var _1iu=E(_1is),_1iv=B(_1iw(_1ir,_1iu[1],_1iu[2],_1it));return [0,_1iv[1],_1iv[2]];},_1ix=function(_1iy,_1iz,_1iA){var _1iB=E(_1iz),_1iC=_1iB[1],_1iD=_1iB[2],_1iE=B(_1iw(_1iy,_1iC,_1iD,_1iA));return [1,E(_1iE[1]),new T(function(){var _1iF=function(_1iG){var _1iH=B(_1iw(_1iy,_1iC,_1iD,_1iG));return [1,E(_1iH[1]),new T(function(){return B(_1iF(_1iH[2]));})];};return B(_1iF(_1iE[2]));})];},_1iI=function(_1iJ,_1iK){var _1iL=function(_1iM){var _1iN=B(_1ii(_1iJ,_1iM));return [1,E(_1iN[1]),new T(function(){return B(_1iL(_1iN[2]));})];};return new F(function(){return _1iL(_1iK);});},_1iO=new T(function(){return [0,_1iq,_1im,_1ix,_1iI,_1hB,_1h7];}),_1iP=function(_1iQ){return E(E(_1iQ)[2]);},_1iR=function(_1iS){return E(E(_1iS)[1]);},_1iT=function(_1iU){return E(E(_1iU)[3]);},_1iV=function(_1iW){return E(E(_1iW)[5]);},_1iX=function(_1iY){return E(E(_1iY)[4]);},_1iZ=[0,E(_XK),E(_1hb)],_1j0=[0,E(_1hb),E(_XK)],_1j1=function(_1j2){return E(E(_1j2)[2]);},_1j3=function(_1j4,_1j5,_1j6,_1j7,_1j8,_1j9,_1ja){while(1){var _1jb=(function(_1jc,_1jd,_1je,_1jf,_1jg,_1jh,_1ji){var _1jj=E(_1jh),_1jk=_1jj[1],_1jl=_1jj[2];if(!B(A(_1iV,[_1je,_1jk,_1jl]))){var _1jm=new T(function(){return B(A(_1j1,[_1jf,_1jg,_1ji]));});return [0,new T(function(){return B(A(_1iP,[_1jd,new T(function(){return B(A(_1iX,[_1jc,_1j0]));}),new T(function(){return B(A(_1iR,[_1jd,new T(function(){return B(A(_1iP,[_1jd,new T(function(){return B(A(_1iX,[_1jc,_1iZ]));}),_1jk]));}),new T(function(){return B(A(_1iP,[_1jd,new T(function(){return E(E(_1jm)[1]);}),new T(function(){return B(A(_1iT,[_1jd,new T(function(){return B(A(_1iP,[_1jd,new T(function(){return B(A(_1iX,[_1jc,_1iZ]));}),_1jl]));}),new T(function(){return B(A(_1iP,[_1jd,new T(function(){return B(A(_1iX,[_1jc,_1iZ]));}),_1jk]));})]));})]));})]));})]));}),new T(function(){return E(E(_1jm)[2]);})];}else{var _1jn=_1jc,_1jo=_1jd,_1jp=_1je,_1jq=_1jf,_1jr=_1jg;_1j9=[0,_1jl,_1jk];var _1js=_1ji;_1j4=_1jn;_1j5=_1jo;_1j6=_1jp;_1j7=_1jq;_1j8=_1jr;_1ja=_1js;return null;}})(_1j4,_1j5,_1j6,_1j7,_1j8,_1j9,_1ja);if(_1jb!=null){return _1jb;}}},_1iw=function(_1jt,_1ju,_1jv,_1jw){var _1jx=B(_1j3(_UK,_UG,_Vj,_1iO,_1jt,[0,_1ju,_1jv],_1jw));return [0,_1jx[1],_1jx[2]];},_1jy=function(_1jz){return E(E(_1jz)[1]);},_1jA=function(_1jB,_1jC,_1jD,_1jE,_1jF){return new F(function(){return A(_1jC,[_1jE,function(_1jG){var _1jH=E(_1jG);return _1jH[0]==0?B(A(_1jD,[[0,new T(function(){return B(A(new T(function(){return B(_1jy(_1jB));}),[function(_1jI){return new F(function(){return _1jA(_1jB,_1jC,_1jD,_1jI,_1jF);});},_1jH[1]]));})]])):B(A(_1jF,[_1jH[1]]));}]);});},_1jJ=function(_1jK,_1jL,_1jM){return [0,_1jL,new T(function(){var _1jN=E(_1jM);if(!_1jN[0]){var _1jO=[0];}else{var _1jP=B(_1jJ(_1jK,new T(function(){return B(A(_1jK,[_1jL,_1jN[1]]));}),_1jN[2])),_1jO=[1,_1jP[1],_1jP[2]];}return _1jO;})];},_1jQ=new T(function(){return B(unCStr("head"));}),_1jR=new T(function(){return B(_ai(_1jQ));}),_1jS=[0,0],_1jT=function(_1jU,_1jV){var _1jW=E(_1jV);return [0,_1jW[1],new T(function(){return B(_SK(_1jW[2],E(_1jU)[2]));})];},_1jX=new T(function(){return B(unCStr("MonadRandom.fromList called with empty list"));}),_1jY=new T(function(){return B(err(_1jX));}),_1jZ=[0,1],_1k0=function(_1k1){var _1k2=I_decodeDouble(_1k1);return [0,[1,_1k2[2]],_1k2[1]];},_1k3=new T(function(){var _1k4=newByteArr(256),_1k5=_1k4,_=_1k5["v"]["i8"][0]=8,_=B((function(_1k6,_1k7,_1k8,_){while(1){if(_1k8>=256){if(_1k6>=256){return E(_);}else{var _1k9=imul(2,_1k6)|0,_1ka=_1k7+1|0,_1kb=_1k6;_1k6=_1k9;_1k7=_1ka;_1k8=_1kb;continue;}}else{var _=_1k5["v"]["i8"][_1k8]=_1k7,_1kb=_1k8+_1k6|0;_1k8=_1kb;continue;}}})(2,0,1,_)),_1kc=_1k5,_1kd=_1kc;return [0,_1kd];}),_1ke=function(_1kf,_1kg){while(1){var _1kh=(function(_1ki,_1kj){var _1kk=hs_int64ToInt(_1ki),_1kl=_1kk,_1km=E(_1k3)[1]["v"]["i8"][(255&_1kl>>>0)>>>0&4294967295];if(_1kj>_1km){if(_1km>=8){var _1kn=hs_uncheckedIShiftRA64(_1ki,8),_1ko=_1kn;_1kf=_1ko;var _1kp=_1kj-8|0;_1kg=_1kp;return null;}else{return [0,new T(function(){var _1kq=hs_uncheckedIShiftRA64(_1ki,_1km),_1kr=_1kq;return B(_Wc(_1kr));}),_1kj-_1km|0];}}else{return [0,new T(function(){var _1ks=hs_uncheckedIShiftRA64(_1ki,_1kj),_1kt=_1ks;return B(_Wc(_1kt));}),0];}})(_1kf,_1kg);if(_1kh!=null){return _1kh;}}},_1ku=function(_1kv){return I_toInt(_1kv)>>>0;},_1kw=function(_1kx){var _1ky=E(_1kx);return _1ky[0]==0?_1ky[1]>>>0:B(_1ku(_1ky[1]));},_1kz=function(_1kA,_1kB){while(1){var _1kC=E(_1kA);if(!_1kC[0]){_1kA=[1,I_fromInt(_1kC[1])];continue;}else{return [1,I_shiftLeft(_1kC[1],_1kB)];}}},_1kD=function(_1kE){var _1kF=B(_1k0(_1kE)),_1kG=_1kF[1],_1kH=_1kF[2];if(_1kH<0){var _1kI=function(_1kJ){if(!_1kJ){return [0,E(_1kG),B(_1kz(_1jZ, -_1kH))];}else{var _1kK=B(_1ke(B(_VL(_1kG)), -_1kH));return [0,E(_1kK[1]),B(_1kz(_1jZ,_1kK[2]))];}};return (B(_1kw(_1kG))&1)>>>0==0?B(_1kI(1)):B(_1kI(0));}else{return [0,B(_1kz(_1kG,_1kH)),_1jZ];}},_1kL=function(_1kM,_){return [1,[0,new T(function(){var _1kN=B(_1kD(E(E(_1kM)[1])[1]));return [0,E(_1kN[1]),E(_1kN[2])];}),new T(function(){return E(E(_1kM)[2]);})]];},_1kO=function(_1kP){return E(E(_1kP)[2]);},_1kQ=[0,0],_1kR=function(_1kS,_1kT){return new F(function(){return (function(_1kU,_1kV){while(1){var _1kW=(function(_1kX,_1kY){var _1kZ=E(_1kX);if(!_1kZ[0]){return E(_1kY);}else{_1kU=_1kZ[2];_1kV=new T(function(){return B(A(new T(function(){return B(_1iR(_1kS));}),[_1kY,_1kZ[1]]));});return null;}})(_1kU,_1kV);if(_1kW!=null){return _1kW;}}})(_1kT,new T(function(){return B(A(_Y8,[_1kS,_1kQ]));}));});},_1l0=[0,1],_1l1=[0,E(_1l0),E(_Jq)],_1l2=function(_1l3,_1l4){return function(_1l5){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(new T(function(){var _1l6=B(_I4(_1l3,0))-1|0;if(0<=_1l6){var _1l7=function(_1l8){return [1,[0,[0,_1l8],_1l1],new T(function(){if(_1l8!=_1l6){var _1l9=B(_1l7(_1l8+1|0));}else{var _1l9=[0];}var _1la=_1l9;return _1la;})];},_1lb=B(_1l7(0));if(!_1lb[0]){var _1lc=E(_1jY);}else{var _1ld=E(_1lb[1]),_1le=E(_1lb[2]);if(!_1le[0]){var _1lf=function(_1lg,_){return [1,[0,_1ld[1],_1lg]];};}else{var _1lf=function(_1lh){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(_1jA(_SE,_nl,_K,function(_){return [1,new T(function(){var _1li=B(_1iw(_Ua,_1jS,new T(function(){var _1lj=B(_1kR(_Th,B(_8X(_1kO,_1lb))));return B(_I1(_1lj[1],_1lj[2]));}),_1lh));return [0,_1li[1],_1li[2]];})];},_1kL));}),function(_1lk,_){return [1,[0,new T(function(){var _1ll=B(_Ks(function(_1lm){return new F(function(){return _Ti(E(_1lm)[2],new T(function(){return E(E(_1lk)[1]);}));});},new T(function(){var _1ln=B(_1jJ(_1jT,_1ld,_1le));return [1,_1ln[1],_1ln[2]];})));if(!_1ll[0]){var _1lo=E(_1jR);}else{var _1lo=E(E(_1ll[1])[1]);}return _1lo;}),new T(function(){return E(E(_1lk)[2]);})]];});});};}var _1lp=_1lf,_1lc=_1lp;}var _1lq=_1lc;}else{var _1lq=E(_1jY);}var _1lr=_1lq,_1ls=_1lr,_1lt=_1ls;return _1lt;}),[_1l5]));}),function(_1lu,_){var _1lv=new T(function(){return E(E(_1lu)[1]);});return [1,[0,[0,new T(function(){var _1lw=E(_1lv)[1],_1lx=new T(function(){return _1lw>=0?B(_M3(_1lw,_1l4)):E(_1l4);});if(_1lw>0){var _1ly=function(_1lz,_1lA){var _1lB=E(_1lz);if(!_1lB[0]){return E(_1lx);}else{var _1lC=_1lB[1];return _1lA>1?[1,_1lC,new T(function(){return B(_1ly(_1lB[2],_1lA-1|0));})]:[1,_1lC,_1lx];}},_1lD=B(_1ly(_1l3,_1lw));}else{var _1lD=E(_1lx);}var _1lE=_1lD,_1lF=_1lE;return _1lF;}),new T(function(){var _1lG=E(_1lv)[1],_1lH=new T(function(){return _1lG>=0?B(_M3(_1lG,_1l3)):E(_1l3);});if(_1lG>0){var _1lI=function(_1lJ,_1lK){var _1lL=E(_1lJ);if(!_1lL[0]){return E(_1lH);}else{var _1lM=_1lL[1];return _1lK>1?[1,_1lM,new T(function(){return B(_1lI(_1lL[2],_1lK-1|0));})]:[1,_1lM,_1lH];}},_1lN=B(_1lI(_1l4,_1lG));}else{var _1lN=E(_1lH);}var _1lO=_1lN,_1lP=_1lO;return _1lP;})],new T(function(){return E(E(_1lu)[2]);})]];});});};},_1lQ=function(_1lR,_1lS){var _1lT=B(_I4(_1lR,0));if(_1lT==B(_I4(_1lS,0))){if(_1lT>1){if(E(_1lT)==2){var _1lU=new T(function(){var _1lV=E(_1lR);if(!_1lV[0]){var _1lW=E(_Sw);}else{var _1lX=E(_1lV[2]),_1lW=_1lX[0]==0?E(_Sw):E(_1lX[2])[0]==0?[0,_1lV[1],_1lX[1]]:E(_Sw);}return _1lW;}),_1lY=new T(function(){var _1lZ=E(_1lS);if(!_1lZ[0]){var _1m0=E(_St);}else{var _1m1=E(_1lZ[2]),_1m0=_1m1[0]==0?E(_St):E(_1m1[2])[0]==0?[0,_1lZ[1],_1m1[1]]:E(_St);}return _1m0;});return function(_1m2,_){return [1,[0,[0,[1,new T(function(){return E(E(_1lU)[1]);}),[1,new T(function(){return E(E(_1lY)[2]);}),_1g]],[1,new T(function(){return E(E(_1lY)[1]);}),[1,new T(function(){return E(E(_1lU)[2]);}),_1g]]],_1m2]];};}else{return new F(function(){return _1l2(_1lR,_1lS);});}}else{return function(_1m3,_){return [1,[0,[0,_1lR,_1lS],_1m3]];};}}else{return E(_Sm);}},_1m4=function(_1m5,_1m6,_1m7){return new F(function(){return _1lQ(_1m6,_1m7);});},_1m8=function(_1m9,_1ma,_){var _1mb=B(A(_1ma,[_])),_1mc=_1mb;return new T(function(){return B(A(_1m9,[_1mc]));});},_1md=function(_1me,_1mf,_1mg,_1mh){return new F(function(){return A(_1mf,[function(_1mi){var _1mj=E(_1mi);return _1mj[0]==0?[0,new T(function(){return B(A(new T(function(){return B(_1jy(_1me));}),[function(_1mk){return new F(function(){return _1md(_1me,_1mf,_1mg,_1mk);});},_1mj[1]]));})]:[1,new T(function(){return B(A(_1mg,[_1mj[1]]));})];},_1mh]);});},_1ml=function(_1mm,_1mn,_1mo){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(_1mm,[_1mo]));}),function(_1mp){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(_1mn,[new T(function(){return E(E(_1mp)[2]);})]));}),function(_1mq,_){return [1,[0,[1,new T(function(){return E(E(_1mp)[1]);}),new T(function(){return E(E(_1mq)[1]);})],new T(function(){return E(E(_1mq)[2]);})]];});});});});},_1mr=function(_1ms,_){return [1,[0,_1g,_1ms]];},_1mt=[0,0],_1mu=function(_1mv){return [0,[1,_1mt,new T(function(){return E(E(_1mv)[1]);})],new T(function(){return E(E(_1mv)[2]);})];},_1mw=function(_1mx,_1my){return function(_1mz){return new F(function(){return _1md(_SE,_1m8,_1mu,new T(function(){return B(A(new T(function(){var _1mA=E(_1mx)[1]-1|0;if(_1mA>0){var _1mB=new T(function(){var _1mC=E(_1my)[1]-1|0;if(-1<=_1mC){var _1mD=function(_1mE){return [1,[0,[0,_1mE],_1l1],new T(function(){if(_1mE!=_1mC){var _1mF=B(_1mD(_1mE+1|0));}else{var _1mF=[0];}var _1mG=_1mF;return _1mG;})];},_1mH=B(_1mD(-1));if(!_1mH[0]){var _1mI=E(_1jY);}else{var _1mJ=E(_1mH[1]),_1mK=E(_1mH[2]);if(!_1mK[0]){var _1mL=function(_1mM,_){return [1,[0,_1mJ[1],_1mM]];};}else{var _1mL=function(_1mN){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(_1jA(_SE,_nl,_K,function(_){return [1,new T(function(){var _1mO=B(_1iw(_Ua,_1jS,new T(function(){var _1mP=B(_1kR(_Th,B(_8X(_1kO,_1mH))));return B(_I1(_1mP[1],_1mP[2]));}),_1mN));return [0,_1mO[1],_1mO[2]];})];},_1kL));}),function(_1mQ,_){return [1,[0,new T(function(){var _1mR=B(_Ks(function(_1mS){return new F(function(){return _Ti(E(_1mS)[2],new T(function(){return E(E(_1mQ)[1]);}));});},new T(function(){var _1mT=B(_1jJ(_1jT,_1mJ,_1mK));return [1,_1mT[1],_1mT[2]];})));if(!_1mR[0]){var _1mU=E(_1jR);}else{var _1mU=E(E(_1mR[1])[1]);}return _1mU;}),new T(function(){return E(E(_1mQ)[2]);})]];});});};}var _1mV=_1mL,_1mI=_1mV;}var _1mW=_1mI;}else{var _1mW=E(_1jY);}var _1mX=_1mW,_1mY=_1mX,_1mZ=_1mY;return _1mZ;}),_1n0=function(_1n1){return _1n1>1?function(_b3){return new F(function(){return _1ml(_1mB,new T(function(){return B(_1n0(_1n1-1|0));}),_b3);});}:function(_1n2){return new F(function(){return _1ml(_1mB,_1mr,_1n2);});};},_1n3=B(_1n0(_1mA));}else{var _1n3=E(_1mr);}var _1n4=_1n3,_1n5=_1n4,_1n6=_1n5;return _1n6;}),[_1mz]));}));});};},_1n7=function(_1n8){var _1n9=E(_1n8);return new F(function(){return _1mw(_1n9[1],_1n9[2]);});},_1na=function(_1nb,_){return [1,[0,_1g,_1nb]];},_1nc=function(_1nd){return [0,_1nd,_1l1];},_1ne=function(_1nf,_1ng){var _1nh=E(_1ng);if(!_1nh[0]){return E(_1na);}else{var _1ni=E(_1nf)[2],_1nj=new T(function(){return B(_8X(_1nc,B(_MD(1,E(_1ni)[1]))));});return function(_1nk){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(new T(function(){var _1nl=B(_I4(_1nh,0))-1|0;if(0<=_1nl){var _1nm=function(_1nn){return [1,[0,[0,_1nn],_1l1],new T(function(){if(_1nn!=_1nl){var _1no=B(_1nm(_1nn+1|0));}else{var _1no=[0];}var _1np=_1no;return _1np;})];},_1nq=B(_1nm(0));if(!_1nq[0]){var _1nr=E(_1jY);}else{var _1ns=E(_1nq[1]),_1nt=E(_1nq[2]);if(!_1nt[0]){var _1nu=function(_1nv,_){return [1,[0,_1ns[1],_1nv]];};}else{var _1nu=function(_1nw){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(_1jA(_SE,_nl,_K,function(_){return [1,new T(function(){var _1nx=B(_1iw(_Ua,_1jS,new T(function(){var _1ny=B(_1kR(_Th,B(_8X(_1kO,_1nq))));return B(_I1(_1ny[1],_1ny[2]));}),_1nw));return [0,_1nx[1],_1nx[2]];})];},_1kL));}),function(_1nz,_){return [1,[0,new T(function(){var _1nA=B(_Ks(function(_1nB){return new F(function(){return _Ti(E(_1nB)[2],new T(function(){return E(E(_1nz)[1]);}));});},new T(function(){var _1nC=B(_1jJ(_1jT,_1ns,_1nt));return [1,_1nC[1],_1nC[2]];})));if(!_1nA[0]){var _1nD=E(_1jR);}else{var _1nD=E(E(_1nA[1])[1]);}return _1nD;}),new T(function(){return E(E(_1nz)[2]);})]];});});};}var _1nE=_1nu,_1nr=_1nE;}var _1nF=_1nr;}else{var _1nF=E(_1jY);}var _1nG=_1nF,_1nH=_1nG,_1nI=_1nH;return _1nI;}),[_1nk]));}),function(_1nJ){var _1nK=new T(function(){return E(E(_1nJ)[1]);});return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){var _1nL=E(_1nK)[1];if(_1nL>=0){var _1nM=E(B(_9S(_1nh,_1nL))[1]);switch(_1nM){case -1:var _1nN=E(_1nj);if(!_1nN[0]){var _1nO=E(_1jY);}else{var _1nP=E(_1nN[1]),_1nQ=E(_1nN[2]);if(!_1nQ[0]){var _1nR=function(_){return [1,[0,_1nP[1],new T(function(){return E(E(_1nJ)[2]);})]];};}else{var _1nR=B(_1jA(_SE,_nl,_K,new T(function(){return B(_1jA(_SE,_nl,_K,function(_){return [1,new T(function(){var _1nS=B(_1iw(_Ua,_1jS,new T(function(){var _1nT=B(_1kR(_Th,B(_8X(_1kO,_1nN))));return B(_I1(_1nT[1],_1nT[2]));}),new T(function(){return E(E(_1nJ)[2]);})));return [0,_1nS[1],_1nS[2]];})];},_1kL));}),function(_1nU,_){return [1,[0,new T(function(){var _1nV=B(_Ks(function(_1nW){return new F(function(){return _Ti(E(_1nW)[2],new T(function(){return E(E(_1nU)[1]);}));});},new T(function(){var _1nX=B(_1jJ(_1jT,_1nP,_1nQ));return [1,_1nX[1],_1nX[2]];})));if(!_1nV[0]){var _1nY=E(_1jR);}else{var _1nY=E(E(_1nV[1])[1]);}return _1nY;}),new T(function(){return E(E(_1nU)[2]);})]];}));}var _1nZ=_1nR,_1nO=_1nZ;}var _1o0=_1nO;break;case 0:var _1o1=E(_1nj);if(!_1o1[0]){var _1o2=E(_1jY);}else{var _1o3=E(_1o1[1]),_1o4=E(_1o1[2]);if(!_1o4[0]){var _1o5=function(_){return [1,[0,_1o3[1],new T(function(){return E(E(_1nJ)[2]);})]];};}else{var _1o5=B(_1jA(_SE,_nl,_K,new T(function(){return B(_1jA(_SE,_nl,_K,function(_){return [1,new T(function(){var _1o6=B(_1iw(_Ua,_1jS,new T(function(){var _1o7=B(_1kR(_Th,B(_8X(_1kO,_1o1))));return B(_I1(_1o7[1],_1o7[2]));}),new T(function(){return E(E(_1nJ)[2]);})));return [0,_1o6[1],_1o6[2]];})];},_1kL));}),function(_1o8,_){return [1,[0,new T(function(){var _1o9=B(_Ks(function(_1oa){return new F(function(){return _Ti(E(_1oa)[2],new T(function(){return E(E(_1o8)[1]);}));});},new T(function(){var _1ob=B(_1jJ(_1jT,_1o3,_1o4));return [1,_1ob[1],_1ob[2]];})));if(!_1o9[0]){var _1oc=E(_1jR);}else{var _1oc=E(E(_1o9[1])[1]);}return _1oc;}),new T(function(){return E(E(_1o8)[2]);})]];}));}var _1od=_1o5,_1o2=_1od;}var _1o0=_1o2;break;default:var _1oe=_1nM-2|0,_1of=function(_1og,_1oh,_1oi,_1oj,_){var _1ok=E(_1oi);if(!_1ok[0]){return [1,[0,_1og,new T(function(){return E(E(_1nJ)[2]);})]];}else{return new F(function(){return A(_1jA,[_SE,_nl,_K,new T(function(){return B(_1jA(_SE,_nl,_K,function(_){return [1,new T(function(){var _1ol=B(_1iw(_Ua,_1jS,new T(function(){var _1om=B(_1kR(_Th,B(_8X(_1kO,_1oj))));return B(_I1(_1om[1],_1om[2]));}),new T(function(){return E(E(_1nJ)[2]);})));return [0,_1ol[1],_1ol[2]];})];},_1kL));}),function(_1on,_){return [1,[0,new T(function(){var _1oo=B(_Ks(function(_1op){return new F(function(){return _Ti(E(_1op)[2],new T(function(){return E(E(_1on)[1]);}));});},new T(function(){var _1oq=B(_1jJ(_1jT,[0,_1og,_1oh],_1ok));return [1,_1oq[1],_1oq[2]];})));if(!_1oo[0]){var _1or=E(_1jR);}else{var _1or=E(E(_1oo[1])[1]);}return _1or;}),new T(function(){return E(E(_1on)[2]);})]];},_]);});}},_1os=new T(function(){var _1ot=E(_1ni)[1],_1ou=_1nM+1|0;if(_1ou<=_1ot){var _1ov=function(_1ow){return [1,[0,[0,_1ow],_1l1],new T(function(){if(_1ow!=_1ot){var _1ox=B(_1ov(_1ow+1|0));}else{var _1ox=[0];}var _1oy=_1ox;return _1oy;})];},_1oz=B(_1ov(_1ou));}else{var _1oz=[0];}var _1oA=_1oz,_1oB=_1oA,_1oC=_1oB;return _1oC;});if(-1<=_1oe){var _1oD=function(_1oE){return [1,[0,[0,_1oE],_1l1],new T(function(){if(_1oE!=_1oe){var _1oF=B(_1oD(_1oE+1|0));}else{var _1oF=E(_1os);}var _1oG=_1oF;return _1oG;})];},_1oH=B(_1oD(-1));if(!_1oH[0]){var _1oI=E(_1jY);}else{var _1oI=function(_){var _1oJ=E(_1oH[1]);return new F(function(){return _1of(_1oJ[1],_1oJ[2],_1oH[2],_1oH,_);});};}var _1oK=_1oI;}else{var _1oL=E(_1os);if(!_1oL[0]){var _1oM=E(_1jY);}else{var _1oM=function(_){var _1oN=E(_1oL[1]);return new F(function(){return _1of(_1oN[1],_1oN[2],_1oL[2],_1oL,_);});};}var _1oK=_1oM;}var _1oO=_1oK,_1oP=_1oO,_1o0=_1oP;}var _1oQ=_1o0,_1oR=_1oQ;}else{var _1oR=E(_9P);}var _1oS=_1oR,_1oT=_1oS;return _1oT;}),function(_1oU,_){return [1,[0,new T(function(){var _1oV=E(_1nK)[1],_1oW=[1,new T(function(){return E(E(_1oU)[1]);}),new T(function(){var _1oX=E(_1nK)[1]+1|0;return _1oX>=0?B(_M3(_1oX,_1nh)):E(_1nh);})];if(_1oV>0){var _1oY=function(_1oZ,_1p0){var _1p1=E(_1oZ);if(!_1p1[0]){return E(_1oW);}else{var _1p2=_1p1[1];return _1p0>1?[1,_1p2,new T(function(){return B(_1oY(_1p1[2],_1p0-1|0));})]:[1,_1p2,_1oW];}},_1p3=B((function(_1p4,_1p5,_1p6){return _1p6>1?[1,_1p4,new T(function(){return B(_1oY(_1p5,_1p6-1|0));})]:[1,_1p4,_1oW];})(_1nh[1],_1nh[2],_1oV));}else{var _1p3=E(_1oW);}var _1p7=_1p3,_1p8=_1p7;return _1p8;}),new T(function(){return E(E(_1oU)[2]);})]];});});});});};}},_1p9=function(_1pa,_1pb){while(1){var _1pc=E(_1pb);if(!_1pc[0]){return 0;}else{var _1pd=B(A(_1pa,[_1pc[1]]));_1pb=_1pc[2];continue;}}},_1pe=function(_1pf){var _1pg=E(_1pf);return 0;},_1ph=function(_1pi){return new F(function(){return _1p9(_1pe,_1pi);});},_1pj=new T(function(){return B(unCStr("CitiesIndivid "));}),_1pk=function(_1pl,_1pm,_1pn){return _1pl<11?B(_5B(_1pj,new T(function(){return B(_dr(_yx,_1pm,_1pn));},1))):[1,_5L,new T(function(){return B(_5B(_1pj,new T(function(){return B(_dr(_yx,_1pm,[1,_5K,_1pn]));},1)));})];},_1po=function(_1pp){return new F(function(){return _1pk(0,_1pp,_1g);});},_1pq=function(_1pr,_1ps){return new F(function(){return _1pk(0,_1pr,_1ps);});},_1pt=function(_1pu,_1n2){return new F(function(){return _dr(_1pq,_1pu,_1n2);});},_1pv=function(_1pw,_1px,_1py){return new F(function(){return _1pk(E(_1pw)[1],_1px,_1py);});},_1pz=[0,_1pv,_1po,_1pt],_1pA=[0,_1pz,_1ph,_1m4,_1ne,_1n7],_1pB=function(_1pC){return E(E(_1pC)[2]);},_1pD=function(_1pE){return E(E(_1pE)[5]);},_1pF=function(_1pG,_){return [1,[0,_1g,_1pG]];},_1pH=function(_1pI,_1pJ,_1pK){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(_1pI,[_1pK]));}),function(_1pL){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(_1pJ,[new T(function(){return E(E(_1pL)[2]);})]));}),function(_1pM,_){return [1,[0,[1,new T(function(){return E(E(_1pL)[1]);}),new T(function(){return E(E(_1pM)[1]);})],new T(function(){return E(E(_1pM)[2]);})]];});});});});},_1pN=function(_1pO,_1pP,_1pQ){if(_1pQ>0){var _1pR=new T(function(){return B(A(_1pD,[_1pO,_1pP]));}),_1pS=function(_1pT){return _1pT>1?function(_b3){return new F(function(){return _1pH(_1pR,new T(function(){return B(_1pS(_1pT-1|0));}),_b3);});}:function(_1pU){return new F(function(){return _1pH(_1pR,_1pF,_1pU);});};};return new F(function(){return _1pS(_1pQ);});}else{return E(_1pF);}},_1pV=function(_1pW,_){var _1pX=E(_1pW);return [1,new T(function(){var _1pY=E(_1pX[1]);return [0,_1pY[1],_1pY[2],_1pY[3],_1pY[4],_1pX[2]];})];},_1pZ=function(_1q0,_1q1,_1q2){while(1){var _1q3=E(_1q2);if(!_1q3[0]){return [0,_1q0,_1q1];}else{var _1q4=_1q3[2],_1q5=E(_1q3[1]),_1q6=_1q5[1],_1q7=_1q5[2];if(!B(_rr(B(_qb(_1q0,_1q7)),B(_qb(_1q6,_1q1))))){_1q2=_1q4;continue;}else{_1q0=_1q6;_1q1=_1q7;_1q2=_1q4;continue;}}}},_1q8=function(_1q9){return E(E(_1q9)[3]);},_1qa=function(_1qb,_1qc,_1qd,_1qe,_1qf){if(!B(_K9(_1qe,_Zu))){var _1qg=function(_1qh){if(!B(_HT(_1qh,_1qf))){return new F(function(){return A(_1qb,[_1qh,new T(function(){return B(_1qg(B(_pT(_1qh,_1qe))));})]);});}else{return E(_1qc);}};return new F(function(){return _1qg(_1qd);});}else{var _1qi=function(_1qj){if(!B(_Kw(_1qj,_1qf))){return new F(function(){return A(_1qb,[_1qj,new T(function(){return B(_1qi(B(_pT(_1qj,_1qe))));})]);});}else{return E(_1qc);}};return new F(function(){return _1qi(_1qd);});}},_1qk=function(_1ql,_1qm){var _1qn=E(_1qm);return _1qn[0]==0?[0]:[1,_1ql,new T(function(){return B(_1qk(_1qn[1],_1qn[2]));})];},_1qo=new T(function(){return B(unCStr("init"));}),_1qp=new T(function(){return B(_ai(_1qo));}),_1qq=function(_1qr,_1qs){var _1qt=E(E(_1qs)[1])[1],_1qu=E(E(_1qr)[1])[1];return _1qt>=_1qu?_1qt!=_1qu?2:1:0;},_1qv=[0,25],_1qw=[0,0],_1qx=function(_1qy,_){return [1,[0,_c,_1qy]];},_1qz=new T(function(){return B(_HL(_1qv,_1qw));}),_1qA=function(_1qB,_){return [1,[0,new T(function(){return B(_jA(E(_1qB)[1]));}),new T(function(){return E(E(_1qB)[2]);})]];},_1qC=new T(function(){return B(unCStr("maximum"));}),_1qD=new T(function(){return B(_ai(_1qC));}),_1qE=function(_1qF){return E(E(_1qF)[4]);},_1qG=[1,_c],_1qH=function(_){return _1qG;},_1qI=[0,_c,_1qH],_1qJ=[0,_1qI],_1qK=function(_){return _1qJ;},_1qL=function(_1qM){return new F(function(){return _1jA(_SE,_nl,_K,_1qK,function(_1qN,_){return [1,[0,_1qN,_1qM]];});});},_1qO=function(_1qP,_1qQ,_1qR){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(_1qP,[_1qR]));}),function(_1qS){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(_1qQ,[new T(function(){return E(E(_1qS)[2]);})]));}),function(_1qT,_){return [1,[0,[1,new T(function(){return E(E(_1qS)[1]);}),new T(function(){return E(E(_1qT)[1]);})],new T(function(){return E(E(_1qT)[2]);})]];});});});});},_1qU=function(_1qV,_){return [1,[0,_1g,_1qV]];},_1qW=function(_1qX){return [0];},_1qY=function(_1qZ){var _1r0=E(_1qZ);return new F(function(){return A(_1r0[1],[_1r0[2]]);});},_1r1=function(_1r2,_){return [1,[0,new T(function(){var _1r3=B(_1kD(E(E(_1r2)[1])[1]));return [0,E(_1r3[1]),E(_1r3[2])];}),new T(function(){return E(E(_1r2)[2]);})]];},_1r4=[0,1],_1r5=function(_1r6,_1r7,_1r8){var _1r9=[1,[0,_1r8,new T(function(){var _1ra=E(_1r6),_1rb=B(_SQ(_1r4,_Jq,_1ra[1],_1ra[2]));return [0,E(_1rb[1]),E(_1rb[2])];})],_1g],_1rc=[0,_1r7,_1r6];return function(_1rd){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(_1jA(_SE,_nl,_K,new T(function(){return B(_1jA(_SE,_nl,_K,function(_){return [1,new T(function(){var _1re=B(_1iw(_Ua,_1jS,new T(function(){var _1rf=B(_1kR(_Th,B(_8X(_1kO,[1,_1rc,_1r9]))));return B(_I1(_1rf[1],_1rf[2]));}),_1rd));return [0,_1re[1],_1re[2]];})];},_1r1));}),function(_1rg,_){return [1,[0,new T(function(){var _1rh=B(_Ks(function(_1ri){return new F(function(){return _Ti(E(_1ri)[2],new T(function(){return E(E(_1rg)[1]);}));});},new T(function(){var _1rj=B(_1jJ(_1jT,_1rc,_1r9));return [1,_1rj[1],_1rj[2]];})));if(!_1rh[0]){var _1rk=E(_1jR);}else{var _1rk=E(E(_1rh[1])[1]);}return _1rk;}),new T(function(){return E(E(_1rg)[2]);})]];}));}),_1qY);});};},_1rl=[1,_1g,_1g],_1rm=function(_1rn,_1ro){var _1rp=function(_1rq,_1rr){var _1rs=E(_1rq);if(!_1rs[0]){return E(_1rr);}else{var _1rt=_1rs[1],_1ru=E(_1rr);if(!_1ru[0]){return E(_1rs);}else{var _1rv=_1ru[1];return B(A(_1rn,[_1rt,_1rv]))==2?[1,_1rv,new T(function(){return B(_1rp(_1rs,_1ru[2]));})]:[1,_1rt,new T(function(){return B(_1rp(_1rs[2],_1ru));})];}}},_1rw=function(_1rx){var _1ry=E(_1rx);if(!_1ry[0]){return [0];}else{var _1rz=E(_1ry[2]);return _1rz[0]==0?E(_1ry):[1,new T(function(){return B(_1rp(_1ry[1],_1rz[1]));}),new T(function(){return B(_1rw(_1rz[2]));})];}},_1rA=function(_1rB){while(1){var _1rC=E(_1rB);if(!_1rC[0]){return E(new T(function(){return B(_1rA(B(_1rw(_1g))));}));}else{if(!E(_1rC[2])[0]){return E(_1rC[1]);}else{_1rB=B(_1rw(_1rC));continue;}}}},_1rD=new T(function(){return B(_1rE(_1g));}),_1rE=function(_1rF){var _1rG=E(_1rF);if(!_1rG[0]){return E(_1rl);}else{var _1rH=_1rG[1],_1rI=E(_1rG[2]);if(!_1rI[0]){return [1,_1rG,_1g];}else{var _1rJ=_1rI[1],_1rK=_1rI[2];if(B(A(_1rn,[_1rH,_1rJ]))==2){return new F(function(){return (function(_1rL,_1rM,_1rN){while(1){var _1rO=(function(_1rP,_1rQ,_1rR){var _1rS=E(_1rR);if(!_1rS[0]){return [1,[1,_1rP,_1rQ],_1rD];}else{var _1rT=_1rS[1];if(B(A(_1rn,[_1rP,_1rT]))==2){_1rL=_1rT;var _1rU=[1,_1rP,_1rQ];_1rN=_1rS[2];_1rM=_1rU;return null;}else{return [1,[1,_1rP,_1rQ],new T(function(){return B(_1rE(_1rS));})];}}})(_1rL,_1rM,_1rN);if(_1rO!=null){return _1rO;}}})(_1rJ,[1,_1rH,_1g],_1rK);});}else{return new F(function(){return (function(_1rV,_1rW,_1rX){while(1){var _1rY=(function(_1rZ,_1s0,_1s1){var _1s2=E(_1s1);if(!_1s2[0]){return [1,new T(function(){return B(A(_1s0,[[1,_1rZ,_1g]]));}),_1rD];}else{var _1s3=_1s2[1],_1s4=_1s2[2];switch(B(A(_1rn,[_1rZ,_1s3]))){case 0:_1rV=_1s3;_1rW=function(_1s5){return new F(function(){return A(_1s0,[[1,_1rZ,_1s5]]);});};_1rX=_1s4;return null;case 1:_1rV=_1s3;_1rW=function(_1s6){return new F(function(){return A(_1s0,[[1,_1rZ,_1s6]]);});};_1rX=_1s4;return null;default:return [1,new T(function(){return B(A(_1s0,[[1,_1rZ,_1g]]));}),new T(function(){return B(_1rE(_1s2));})];}}})(_1rV,_1rW,_1rX);if(_1rY!=null){return _1rY;}}})(_1rJ,function(_1s7){return [1,_1rH,_1s7];},_1rK);});}}}};return new F(function(){return _1rA(B(_1rE(_1ro)));});},_1s8=function(_1s9,_1sa){var _1sb=E(_1s9);if(!_1sb){return [0];}else{var _1sc=E(_1sa);return _1sc[0]==0?[0]:[1,_1sc[1],new T(function(){return B(_1s8(_1sb-1|0,_1sc[2]));})];}},_1sd=function(_1se,_1sf,_1sg,_1sh,_1si){var _1sj=new T(function(){return B(_1qE(_1se));}),_1sk=new T(function(){var _1sl=B(_8X(_1kO,B(_1rm(_1qq,B(_84(function(_1sm,_1sn,_1so){return [1,[0,new T(function(){return B(A(_1sg,[_1sm]));}),_1sn],_1so];},_1g,_1si,_1si)))))),_1sp=B(_I4(_1sl,0))*E(E(_1sh)[2])[1],_1sq=_1sp&4294967295;if(_1sq>=_1sp){if(_1sq>0){var _1sr=_1sq<0?[0]:B(_1s8(_1sq,_1sl));}else{var _1sr=[0];}var _1ss=_1sr,_1st=_1ss;}else{var _1su=_1sq+1|0;if(_1su>0){var _1sv=_1su<0?[0]:B(_1s8(_1su,_1sl));}else{var _1sv=[0];}var _1sw=_1sv,_1sx=_1sw,_1st=_1sx;}var _1sy=_1st,_1sz=_1sy,_1sA=_1sz,_1sB=_1sA,_1sC=_1sB,_1sD=_1sC,_1sE=_1sD,_1sF=_1sE,_1sG=_1sF,_1sH=_1sG,_1sI=_1sH;return _1sI;});return function(_1sJ){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(_1jA(_SE,_nl,_K,new T(function(){return B(A(new T(function(){var _1sK=B(_1k0( -((B(_I4(_1si,0))-B(_I4(_1sk,0))|0)/2))),_1sL=_1sK[1],_1sM=_1sK[2],_1sN=new T(function(){var _1sO=B(_1kD(E(E(_1sh)[1])[1]));return [0,E(_1sO[1]),E(_1sO[2])];}),_1sP=new T(function(){var _1sQ=B(_8X(function(_1sR){var _1sS=B(_1kD(B(A(_1sg,[_1sR]))[1]));return [0,E(_1sS[1]),E(_1sS[2])];},_1si)),_1sT=function(_1sU){var _1sV=E(_1sU);return _1sV[0]==0?E(_1qW):function(_1sW){var _1sX=E(_1sW);return _1sX[0]==0?[0]:[1,[0,_1sX[1],new T(function(){var _1sY=E(_1sV[1]),_1sZ=E(new T(function(){var _1t0=E(_1sQ);if(!_1t0[0]){var _1t1=E(_1qD);}else{var _1t2=E(_1t0[1]),_1t3=B(_1pZ(_1t2[1],_1t2[2],_1t0[2])),_1t1=[0,E(_1t3[1]),E(_1t3[2])];}return _1t1;})),_1t4=B(_YS(_1sY[1],_1sY[2],_1sZ[1],_1sZ[2]));return [0,E(_1t4[1]),E(_1t4[2])];})],new T(function(){return B(A(new T(function(){return B(_1sT(_1sV[2]));}),[_1sX[2]]));})];};},_1t5=B(A(_1sT,[_1sQ,_1si]));if(!_1t5[0]){var _1t6=E(_1jY);}else{var _1t7=E(_1t5[1]),_1t8=E(_1t5[2]);if(!_1t8[0]){var _1t9=function(_1ta,_){return [1,[0,_1t7[1],_1ta]];};}else{var _1t9=function(_1tb){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(_1jA(_SE,_nl,_K,function(_){return [1,new T(function(){var _1tc=B(_1iw(_Ua,_1jS,new T(function(){var _1td=B(_1kR(_Th,B(_8X(_1kO,_1t5))));return B(_I1(_1td[1],_1td[2]));}),_1tb));return [0,_1tc[1],_1tc[2]];})];},_1r1));}),function(_1te,_){return [1,[0,new T(function(){var _1tf=B(_Ks(function(_1tg){return new F(function(){return _Ti(E(_1tg)[2],new T(function(){return E(E(_1te)[1]);}));});},new T(function(){var _1th=B(_1jJ(_1jT,_1t7,_1t8));return [1,_1th[1],_1th[2]];})));if(!_1tf[0]){var _1ti=E(_1jR);}else{var _1ti=E(E(_1tf[1])[1]);}return _1ti;}),new T(function(){return E(E(_1te)[2]);})]];});});};}var _1tj=_1t9,_1t6=_1tj;}var _1tk=_1t6;return _1tk;}),_1tl=function(_1tm){return function(_1tn){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(new T(function(){if(!E(_1qz)){var _1to=!B(_HL(B(_Yc(_1tm,_1qv)),_1qw))?E(_1qx):E(_1qL);}else{var _1to=E(_IO);}return _1to;}),[_1tn]));}),function(_1tp){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(_1sP,[new T(function(){return E(E(_1tp)[2]);})]));}),function(_1tq){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(_1sP,[new T(function(){return E(E(_1tq)[2]);})]));}),function(_1tr){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(new T(function(){return B(_1q8(_1se));}),[_1sf,new T(function(){return E(E(_1tq)[1]);}),new T(function(){return E(E(_1tr)[1]);}),new T(function(){return E(E(_1tr)[2]);})]));}),function(_1ts){var _1tt=E(_1ts),_1tu=E(_1tt[1]),_1tv=_1tu[1],_1tw=_1tu[2];return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(_1jA(_SE,_nl,_K,new T(function(){var _1tx=[1,[0,function(_1ty,_){return [1,[0,_1tv,_1ty]];},new T(function(){var _1tz=E(_1sN),_1tA=B(_SQ(_1r4,_Jq,_1tz[1],_1tz[2]));return [0,E(_1tA[1]),E(_1tA[2])];})],_1g],_1tB=[0,new T(function(){return B(A(_1sj,[_1sf,_1tv]));}),_1sN];return B(_1jA(_SE,_nl,_K,new T(function(){return B(_1jA(_SE,_nl,_K,function(_){return [1,new T(function(){var _1tC=B(_1iw(_Ua,_1jS,new T(function(){var _1tD=B(_1kR(_Th,B(_8X(_1kO,[1,_1tB,_1tx]))));return B(_I1(_1tD[1],_1tD[2]));}),_1tt[2]));return [0,_1tC[1],_1tC[2]];})];},_1r1));}),function(_1tE,_){return [1,[0,new T(function(){var _1tF=B(_Ks(function(_1tG){return new F(function(){return _Ti(E(_1tG)[2],new T(function(){return E(E(_1tE)[1]);}));});},new T(function(){var _1tH=B(_1jJ(_1jT,_1tB,_1tx));return [1,_1tH[1],_1tH[2]];})));if(!_1tF[0]){var _1tI=E(_1jR);}else{var _1tI=E(E(_1tF[1])[1]);}return _1tI;}),new T(function(){return E(E(_1tE)[2]);})]];}));}),_1qY));}),function(_1tJ){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(new T(function(){return B(_1r5(_1sN,new T(function(){return B(A(_1sj,[_1sf,_1tw]));}),function(_1tK,_){return [1,[0,_1tw,_1tK]];}));}),[new T(function(){return E(E(_1tJ)[2]);})]));}),function(_1tL,_){return [1,[0,[1,new T(function(){return E(E(_1tJ)[1]);}),[1,new T(function(){return E(E(_1tL)[1]);}),_1g]],new T(function(){return E(E(_1tL)[2]);})]];});});});});});});});});});});});});};};if(_1sM>=0){var _1tM=B(_1qa(function(_1tN,_1tO){return function(_b3){return new F(function(){return _1qO(new T(function(){return B(_1tl(_1tN));}),_1tO,_b3);});};},_1qU,_1r4,_1r4,B(_q3(B(_1kz(_1sL,_1sM))))));}else{var _1tP= -_1sM;if(_1tP<=52){var _1tQ=hs_uncheckedIShiftRA64(B(_VL(_1sL)),_1tP),_1tR=_1tQ,_1tS=B(_1qa(function(_1tT,_1tU){return function(_b3){return new F(function(){return _1qO(new T(function(){return B(_1tl(_1tT));}),_1tU,_b3);});};},_1qU,_1r4,_1r4,B(_q3(B(_Wc(_1tR))))));}else{if(!B(_HT(_1sL,_1qw))){var _1tV=B(_1qa(function(_1tW,_1tX){return function(_b3){return new F(function(){return _1qO(new T(function(){return B(_1tl(_1tW));}),_1tX,_b3);});};},_1qU,_1r4,_1r4,_1qw));}else{var _1tV=B(_1qa(function(_1tY,_1tZ){return function(_b3){return new F(function(){return _1qO(new T(function(){return B(_1tl(_1tY));}),_1tZ,_b3);});};},_1qU,_1r4,_1r4,_1r4));}var _1tS=_1tV;}var _1u0=_1tS,_1u1=_1u0,_1tM=_1u1;}var _1u2=_1tM,_1u3=_1u2,_1u4=_1u3,_1u5=_1u4,_1u6=_1u5,_1u7=_1u6,_1u8=_1u7,_1u9=_1u8;return _1u9;}),[_1sJ]));}),_1qA));}),function(_1ua,_){return [1,[0,new T(function(){var _1ub=B(_5B(_1sk,new T(function(){return E(E(_1ua)[1]);},1)));if(B(_I4(_1ub,0))>E(new T(function(){return [0,B(_I4(_1si,0))];}))[1]){var _1uc=E(_1ub),_1ud=_1uc[0]==0?E(_1qp):B(_1qk(_1uc[1],_1uc[2]));}else{var _1ud=E(_1ub);}var _1ue=_1ud,_1uf=_1ue,_1ug=_1uf,_1uh=_1ug;return _1uh;}),new T(function(){return E(E(_1ua)[2]);})]];});});};},_1ui=function(_1uj,_1uk,_1ul){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(_1uj,[_1ul]));}),function(_1um){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(_1uk,[new T(function(){return E(E(_1um)[2]);})]));}),function(_1un,_){return [1,[0,[1,new T(function(){return E(E(_1um)[1]);}),new T(function(){return E(E(_1un)[1]);})],new T(function(){return E(E(_1un)[2]);})]];});});});});},_1uo=function(_1up,_){return [1,[0,_1g,_1up]];},_1uq=function(_1ur,_1us,_1ut,_1uu,_1uv,_1uw,_1ux,_1uy,_1uz,_){return !E(_1uv)?B(A(_1jA,[_SE,_nl,_K,new T(function(){var _1uA=function(_1uB){var _1uC=E(_1uB);return _1uC[0]==0?E(_1uo):function(_1uD){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(_1jA(_SE,_nl,_K,new T(function(){return B(_1qL(_1uD));}),function(_1uE){return new F(function(){return A(new T(function(){return B(_1sd(_1ur,_1us,_1ut,_1uu,_1uC[1]));}),[new T(function(){return E(E(_1uE)[2]);})]);});}));}),function(_1uF){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(A(new T(function(){return B(_1uA(_1uC[2]));}),[new T(function(){return E(E(_1uF)[2]);})]));}),function(_1uG,_){return [1,[0,[1,new T(function(){return E(E(_1uF)[1]);}),new T(function(){return E(E(_1uG)[1]);})],new T(function(){return E(E(_1uG)[2]);})]];});});});});};};return B(_1jA(_SE,_nl,_K,new T(function(){if(!E(E(_1uw)[1])){var _1uH=E(_1uu),_1uI=E(_1uH[4])[1];if(_1uI>0){var _1uJ=function(_1uK){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){return B(_1qL(_1uK));}),function(_1uL){return new F(function(){return A(new T(function(){return B(_1pN(_1ur,_1us,E(_1uH[5])[1]));}),[new T(function(){return E(E(_1uL)[2]);})]);});});});},_1uM=function(_1uN){return _1uN>1?function(_b3){return new F(function(){return _1ui(_1uJ,new T(function(){return B(_1uM(_1uN-1|0));}),_b3);});}:function(_1uO){return new F(function(){return _1ui(_1uJ,_1uo,_1uO);});};},_1uP=B(A(_1uM,[_1uI,_1uz]));}else{var _1uP=function(_){return [1,[0,_1g,_1uz]];};}var _1uQ=_1uP,_1uR=_1uQ,_1uS=_1uR,_1uT=_1uS;}else{var _1uT=function(_){return [1,[0,_1ux,_1uz]];};}var _1uU=_1uT;return _1uU;}),function(_1uV){return new F(function(){return _1jA(_SE,_nl,_K,new T(function(){var _1uW=new T(function(){return E(E(_1uV)[1]);}),_1uX=B(_1p9(function(_1uO){return new F(function(){return _1p9(new T(function(){return B(_1pB(_1ur));}),_1uO);});},_1uW));return B(A(_1uA,[_1uW,new T(function(){return E(E(_1uV)[2]);})]));}),function(_1uY,_){var _1uZ=new T(function(){return E(E(_1uY)[1]);}),_1v0=new T(function(){return B(_91(_1ut,_1uZ));});return [1,[0,[0,new T(function(){var _1v1=E(_1v0);if(!_1v1[0]){var _1v2=false;}else{if(!E(new T(function(){return (E(_1uw)[1]+1|0)>=E(E(_1uu)[3])[1];}))){var _1v3=E(E(_1uu)[6]);if(!_1v3[0]){var _1v4=false;}else{var _1v4=E(E(_1v1[1])[1])[1]>=E(_1v3[1])[1];}var _1v5=_1v4,_1v6=_1v5;}else{var _1v6=true;}var _1v2=_1v6;}return _1v2;}),new T(function(){return [0,E(_1uw)[1]+1|0];}),_1uZ,_1v0,_1uz],new T(function(){return E(E(_1uY)[2]);})]];});});}));}),_1pV,_])):[1,[0,_fI,_1uw,_1ux,_1uy,_1uz]];},_1v7=function(_1v8,_1v9,_1va){while(1){var _1vb=E(_1va);if(!_1vb[0]){return [0,_1v8,[0,_1v9]];}else{var _1vc=_1vb[2],_1vd=E(_1vb[1]),_1ve=E(_1vd[2])[1];if(_1v9>=_1ve){if(_1v9!=_1ve){_1v8=_1vd[1];_1v9=_1ve;_1va=_1vc;continue;}else{_1va=_1vc;continue;}}else{_1va=_1vc;continue;}}}},_1vf=function(_1vg,_1vh,_1vi){while(1){var _1vj=E(_1vi);if(!_1vj[0]){return [0,_1vg,[0,_1vh]];}else{var _1vk=_1vj[2],_1vl=E(_1vj[1]),_1vm=_1vl[1],_1vn=E(_1vl[2])[1];if(_1vh>=_1vn){if(_1vh!=_1vn){_1vi=_1vk;continue;}else{_1vg=_1vm;_1vh=_1vn;_1vi=_1vk;continue;}}else{_1vg=_1vm;_1vh=_1vn;_1vi=_1vk;continue;}}}},_1vo=function(_1vp,_1vq,_1vr){while(1){var _1vs=E(_1vr);if(!_1vs[0]){return [0,[0,_1vp],_1vq];}else{var _1vt=_1vs[2],_1vu=E(_1vs[1]),_1vv=E(_1vu[1])[1];if(_1vp>=_1vv){if(_1vp!=_1vv){_1vp=_1vv;_1vq=_1vu[2];_1vr=_1vt;continue;}else{_1vr=_1vt;continue;}}else{_1vr=_1vt;continue;}}}},_1vw=function(_1vx,_1vy,_1vz){while(1){var _1vA=E(_1vz);if(!_1vA[0]){return [0,[0,_1vx],_1vy];}else{var _1vB=_1vA[2],_1vC=E(_1vA[1]),_1vD=_1vC[2],_1vE=E(_1vC[1])[1];if(_1vx>=_1vE){if(_1vx!=_1vE){_1vz=_1vB;continue;}else{_1vx=_1vE;_1vy=_1vD;_1vz=_1vB;continue;}}else{_1vx=_1vE;_1vy=_1vD;_1vz=_1vB;continue;}}}},_1vF=function(_1vG,_1vH,_1vI,_1vJ,_){var _1vK=jsMoveTo(_1vJ,_1vG+_1vI,_1vH),_1vL=jsArc(_1vJ,_1vG,_1vH,_1vI,0,6.283185307179586);return _c;},_1vM=function(_1vN,_1vO,_){var _1vP=jsBeginPath(_1vO),_1vQ=B(A(_1vN,[[0,_1vO],_])),_1vR=_1vQ,_1vS=jsFill(_1vO);return _c;},_1vT=function(_1vU,_1vV){var _1vW=_1vU-1|0;if(_1vW>=0){var _1vX=B(_M3(_1vW,_1vV));return _1vX[0]==0?[0]:[1,_1vX[1],new T(function(){return B(_1vT(_1vU,_1vX[2]));})];}else{var _1vY=E(_1vV);return _1vY[0]==0?[0]:[1,_1vY[1],new T(function(){return B(_1vT(_1vU,_1vY[2]));})];}},_1vZ=new T(function(){return B(unCStr("n is larger than passed list!"));}),_1w0=new T(function(){return B(err(_1vZ));}),_1w1=function(_1w2,_1w3){var _1w4=B(_I4(_1w3,0));return _1w2<=_1w4?_1w2>0?_1w2<0?[0]:B(_1s8(_1w2,new T(function(){var _1w5=jsRound(_1w4/_1w2),_1w6=_1w5;return B(_1vT(_1w6,_1w3));},1))):[0]:E(_1w0);},_1w7=function(_1w8,_){return _c;},_1w9=function(_1wa,_){return _1g;},_1wb=new T(function(){return [0,toJSStr(_1g)];}),_1wc=new T(function(){return [0,"rgb("];}),_1wd=[0,44],_1we=[1,_1wd,_1g],_1wf=new T(function(){return [0,toJSStr(_1we)];}),_1wg=new T(function(){return [0,"rgba("];}),_1wh=[0,41],_1wi=[1,_1wh,_1g],_1wj=new T(function(){return [0,toJSStr(_1wi)];}),_1wk=[1,_1wj,_1g],_1wl=function(_1wm){var _1wn=E(_1wm);if(!_1wn[0]){var _1wo=jsCat([1,_1wc,[1,new T(function(){var _1wp=String(_1wn[1]),_1wq=_1wp;return [0,_1wq];}),[1,_1wf,[1,new T(function(){var _1wr=String(_1wn[2]),_1ws=_1wr;return [0,_1ws];}),[1,_1wf,[1,new T(function(){var _1wt=String(_1wn[3]),_1wu=_1wt;return [0,_1wu];}),_1wk]]]]]],E(_1wb)[1]),_1wv=_1wo;return E(_1wv);}else{var _1ww=jsCat([1,_1wg,[1,new T(function(){var _1wx=String(_1wn[1]),_1wy=_1wx;return [0,_1wy];}),[1,_1wf,[1,new T(function(){var _1wz=String(_1wn[2]),_1wA=_1wz;return [0,_1wA];}),[1,_1wf,[1,new T(function(){var _1wB=String(_1wn[3]),_1wC=_1wB;return [0,_1wC];}),[1,_1wf,[1,new T(function(){var _1wD=String(_1wn[4]),_1wE=_1wD;return [0,_1wE];}),_1wk]]]]]]]],E(_1wb)[1]),_1wF=_1ww;return E(_1wF);}},_1wG=new T(function(){return [0,"strokeStyle"];}),_1wH=new T(function(){return [0,"fillStyle"];}),_1wI=function(_1wJ,_1wK){return function(_1wL,_){var _1wM=E(_1wL),_1wN=_1wM[1],_1wO=E(_1wH)[1],_1wP=jsGet(_1wN,_1wO),_1wQ=_1wP,_1wR=E(_1wG)[1],_1wS=jsGet(_1wN,_1wR),_1wT=_1wS,_1wU=E(new T(function(){return [0,B(_1wl(_1wJ))];}))[1],_1wV=jsSet(_1wN,_1wO,_1wU),_1wW=jsSet(_1wN,_1wR,_1wU),_1wX=B(A(_1wK,[_1wM,_])),_1wY=_1wX,_1wZ=jsSet(_1wN,_1wO,_1wQ),_1x0=jsSet(_1wN,_1wR,_1wT);return _c;};},_1x1=function(_1x2,_){return _c;},_1x3=function(_1x4){var _1x5=E(_1x4);if(!_1x5[0]){return E(_1x1);}else{var _1x6=E(_1x5[1]);return function(_1x7,_){var _1x8=E(_1x7)[1],_1x9=jsMoveTo(_1x8,E(_1x6[1])[1],E(_1x6[2])[1]);return new F(function(){return (function(_1xa,_){while(1){var _1xb=E(_1xa);if(!_1xb[0]){return _c;}else{var _1xc=E(_1xb[1]),_1xd=jsLineTo(_1x8,E(_1xc[1])[1],E(_1xc[2])[1]);_1xa=_1xb[2];continue;}}})(_1x5[2],_);});};}},_1xe=function(_1xf){var _1xg=E(_1xf);if(!_1xg[0]){return E(_1w7);}else{var _1xh=_1xg[1];return function(_1xi,_){var _1xj=E(_1xi),_1xk=_1xj[1],_1xl=jsBeginPath(_1xk),_1xm=B(A(new T(function(){return B(_1x3([1,new T(function(){return E(E(_1xh)[1]);}),[1,new T(function(){return E(E(_1xh)[2]);}),_1g]]));}),[[0,_1xk],_])),_1xn=_1xm,_1xo=jsStroke(_1xk);return new F(function(){return A(new T(function(){return B(_1xe(_1xg[2]));}),[_1xj,_]);});};}},_1xp=function(_1xq){var _1xr=E(_1xq);if(!_1xr[0]){return E(_1w7);}else{var _1xs=_1xr[1];return function(_1xt,_){var _1xu=E(_1xt),_1xv=_1xu[1],_1xw=jsBeginPath(_1xv),_1xx=B(A(new T(function(){return B(_1x3([1,new T(function(){return E(E(_1xs)[1]);}),[1,new T(function(){return E(E(_1xs)[2]);}),_1g]]));}),[[0,_1xv],_])),_1xy=_1xx,_1xz=jsStroke(_1xv);return new F(function(){return A(new T(function(){return B(_1xp(_1xr[2]));}),[_1xu,_]);});};}},_1xA=function(_1xB,_1xC){while(1){var _1xD=E(_1xC);if(!_1xD[0]){return E(_1xB);}else{_1xB=_1xD[1];_1xC=_1xD[2];continue;}}},_1xE=new T(function(){return B(unCStr("last"));}),_1xF=new T(function(){return B(_ai(_1xE));}),_1xG=[0,2],_1xH=[0,0],_1xI=[0,_1xH,_1xH],_1xJ=[1,_1xI,_1g],_1xK=[0,1],_1xL=[0,125,125,125],_1xM=[0,0],_1xN=new T(function(){return B(unCStr("List.minimumBy: empty list"));}),_1xO=new T(function(){return B(err(_1xN));}),_1xP=function(_1xQ){return E(_1xQ);},_1xR=[0,102],_1xS=[1,_1xR,_1g],_1xT=function(_1xU,_1xV){while(1){var _1xW=E(_1xU);if(!_1xW[0]){return E(_1xV);}else{_1xU=_1xW[2];var _1xX=[1,_1xW[1],_1xV];_1xV=_1xX;continue;}}},_1xY=function(_1xZ){var _1y0=E(_1xZ)[1];return [0,Math.log(_1y0+(_1y0+1)*Math.sqrt((_1y0-1)/(_1y0+1)))];},_1y1=function(_1y2){var _1y3=E(_1y2)[1];return [0,Math.log(_1y3+Math.sqrt(1+_1y3*_1y3))];},_1y4=function(_1y5){var _1y6=E(_1y5)[1];return [0,0.5*Math.log((1+_1y6)/(1-_1y6))];},_1y7=function(_1y8,_1y9){return [0,Math.log(E(_1y9)[1])/Math.log(E(_1y8)[1])];},_1ya=[0,3.141592653589793],_1yb=function(_1yc){return [0,Math.acos(E(_1yc)[1])];},_1yd=function(_1ye){return [0,Math.asin(E(_1ye)[1])];},_1yf=function(_1yg){return [0,Math.atan(E(_1yg)[1])];},_1yh=function(_1yi){return [0,Math.cos(E(_1yi)[1])];},_1yj=function(_1yk){return [0,cosh(E(_1yk)[1])];},_1yl=function(_1ym){return [0,Math.exp(E(_1ym)[1])];},_1yn=function(_1yo){return [0,Math.log(E(_1yo)[1])];},_1yp=function(_1yq,_1yr){return [0,Math.pow(E(_1yq)[1],E(_1yr)[1])];},_1ys=function(_1yt){return [0,Math.sin(E(_1yt)[1])];},_1yu=function(_1yv){return [0,sinh(E(_1yv)[1])];},_1yw=function(_1yx){return [0,Math.sqrt(E(_1yx)[1])];},_1yy=function(_1yz){return [0,Math.tan(E(_1yz)[1])];},_1yA=function(_1yB){return [0,tanh(E(_1yB)[1])];},_1yC=[0,_UK,_1ya,_1yl,_1yw,_1yn,_1yp,_1y7,_1ys,_1yy,_1yh,_1yd,_1yf,_1yb,_1yu,_1yA,_1yj,_1y1,_1y4,_1xY],_1yD=function(_1yE){var _1yF=E(_1yE)[1];return [0,Math.log(_1yF+(_1yF+1)*Math.sqrt((_1yF-1)/(_1yF+1)))];},_1yG=function(_1yH){var _1yI=E(_1yH)[1];return [0,Math.log(_1yI+Math.sqrt(1+_1yI*_1yI))];},_1yJ=function(_1yK){var _1yL=E(_1yK)[1];return [0,0.5*Math.log((1+_1yL)/(1-_1yL))];},_1yM=function(_1yN,_1yO){return [0,Math.log(E(_1yO)[1])/Math.log(E(_1yN)[1])];},_1yP=[0,3.141592653589793],_1yQ=new T(function(){return [0,0/0];}),_1yR=new T(function(){return [0,-1/0];}),_1yS=new T(function(){return [0,1/0];}),_1yT=function(_1yU,_1yV){return !B(_HL(_1yV,_HF))?[0,B(_HG(_1yU,_1yV))]:!B(_HL(_1yU,_HF))?!B(_HT(_1yU,_HF))?E(_1yS):E(_1yR):E(_1yQ);},_1yW=function(_1yX){var _1yY=E(_1yX);return new F(function(){return _1yT(_1yY[1],_1yY[2]);});},_1yZ=function(_1z0){return [0,1/E(_1z0)[1]];},_1z1=function(_1z2){var _1z3=E(_1z2),_1z4=_1z3[1];return _1z4<0?[0, -_1z4]:E(_1z3);},_1z5=function(_1z6){var _1z7=E(_1z6);return _1z7[0]==0?_1z7[1]:I_toNumber(_1z7[1]);},_1z8=function(_1z9){return [0,B(_1z5(_1z9))];},_1za=[0,0],_1zb=[0,1],_1zc=[0,-1],_1zd=function(_1ze){var _1zf=E(E(_1ze)[1]);return _1zf==0?E(_1za):_1zf<=0?E(_1zc):E(_1zb);},_1zg=function(_1zh,_1zi){return [0,E(_1zh)[1]-E(_1zi)[1]];},_1zj=function(_1zk){return [0, -E(_1zk)[1]];},_1zl=function(_1zm,_1zn){return [0,E(_1zm)[1]+E(_1zn)[1]];},_1zo=function(_1zp,_1zq){return [0,E(_1zp)[1]*E(_1zq)[1]];},_1zr=[0,_1zl,_1zo,_1zg,_1zj,_1z1,_1zd,_1z8],_1zs=function(_1zt,_1zu){return [0,E(_1zt)[1]/E(_1zu)[1]];},_1zv=[0,_1zr,_1zs,_1yZ,_1yW],_1zw=function(_1zx){return [0,Math.acos(E(_1zx)[1])];},_1zy=function(_1zz){return [0,Math.asin(E(_1zz)[1])];},_1zA=function(_1zB){return [0,Math.atan(E(_1zB)[1])];},_1zC=function(_1zD){return [0,Math.cos(E(_1zD)[1])];},_1zE=function(_1zF){return [0,cosh(E(_1zF)[1])];},_1zG=function(_1zH){return [0,Math.exp(E(_1zH)[1])];},_1zI=function(_1zJ){return [0,Math.log(E(_1zJ)[1])];},_1zK=function(_1zL,_1zM){return [0,Math.pow(E(_1zL)[1],E(_1zM)[1])];},_1zN=function(_1zO){return [0,Math.sin(E(_1zO)[1])];},_1zP=function(_1zQ){return [0,sinh(E(_1zQ)[1])];},_1zR=function(_1zS){return [0,Math.sqrt(E(_1zS)[1])];},_1zT=function(_1zU){return [0,Math.tan(E(_1zU)[1])];},_1zV=function(_1zW){return [0,tanh(E(_1zW)[1])];},_1zX=[0,_1zv,_1yP,_1zG,_1zR,_1zI,_1zK,_1yM,_1zN,_1zT,_1zC,_1zy,_1zA,_1zw,_1zP,_1zV,_1zE,_1yG,_1yJ,_1yD],_1zY=function(_1zZ){var _1A0=B(_1k0(E(_1zZ)[1]));return [0,_1A0[1],[0,_1A0[2]]];},_1A1=[0,53],_1A2=function(_1A3){return E(_1A1);},_1A4=[0,2],_1A5=function(_1A6){return E(_1A4);},_1A7=[0,_HB,_HA],_1A8=function(_1A9){return E(_1A7);},_1Aa=function(_1Ab){var _1Ac=isDoubleInfinite(E(_1Ab)[1]),_1Ad=_1Ac;return E(_1Ad)==0?false:true;},_1Ae=function(_1Af){var _1Ag=isDoubleNaN(E(_1Af)[1]),_1Ah=_1Ag;return E(_1Ah)==0?false:true;},_1Ai=function(_1Aj){var _1Ak=isDoubleNegativeZero(E(_1Aj)[1]),_1Al=_1Ak;return E(_1Al)==0?false:true;},_1Am=function(_1An){var _1Ao=decodeFloat(E(_1An)[1]);return [0,new T(function(){return B(_q9(_1Ao[1]));}),[0,_1Ao[2]]];},_1Ap=[0,24],_1Aq=function(_1Ar){return E(_1Ap);},_1As=function(_1At){return E(_1A4);},_1Au=[0,128],_1Av=[0,-125],_1Aw=[0,_1Av,_1Au],_1Ax=function(_1Ay){return E(_1Aw);},_1Az=function(_1AA){var _1AB=isFloatInfinite(E(_1AA)[1]),_1AC=_1AB;return E(_1AC)==0?false:true;},_1AD=function(_1AE){var _1AF=isFloatNaN(E(_1AE)[1]),_1AG=_1AF;return E(_1AG)==0?false:true;},_1AH=function(_1AI){var _1AJ=isFloatNegativeZero(E(_1AI)[1]),_1AK=_1AJ;return E(_1AK)==0?false:true;},_1AL=function(_1AM){var _1AN=B(_1kD(E(_1AM)[1]));return [0,E(_1AN[1]),E(_1AN[2])];},_1AO=[0,_UG,_Vj,_1AL],_1AP=function(_1AQ){return new F(function(){return _MD(E(_1AQ)[1],2147483647);});},_1AR=function(_1AS,_1AT,_1AU){return _1AU<=_1AT?[1,[0,_1AS],new T(function(){var _1AV=_1AT-_1AS|0,_1AW=function(_1AX){return _1AX>=(_1AU-_1AV|0)?[1,[0,_1AX],new T(function(){return B(_1AW(_1AX+_1AV|0));})]:[1,[0,_1AX],_1g];};return B(_1AW(_1AT));})]:_1AU<=_1AS?[1,[0,_1AS],_1g]:[0];},_1AY=function(_1AZ,_1B0,_1B1){return _1B1>=_1B0?[1,[0,_1AZ],new T(function(){var _1B2=_1B0-_1AZ|0,_1B3=function(_1B4){return _1B4<=(_1B1-_1B2|0)?[1,[0,_1B4],new T(function(){return B(_1B3(_1B4+_1B2|0));})]:[1,[0,_1B4],_1g];};return B(_1B3(_1B0));})]:_1B1>=_1AZ?[1,[0,_1AZ],_1g]:[0];},_1B5=function(_1B6,_1B7){return _1B7<_1B6?B(_1AR(_1B6,_1B7,-2147483648)):B(_1AY(_1B6,_1B7,2147483647));},_1B8=function(_1B9,_1Ba){return new F(function(){return _1B5(E(_1B9)[1],E(_1Ba)[1]);});},_1Bb=function(_1Bc,_1Bd,_1Be){return _1Bd<_1Bc?B(_1AR(_1Bc,_1Bd,_1Be)):B(_1AY(_1Bc,_1Bd,_1Be));},_1Bf=function(_1Bg,_1Bh,_1Bi){return new F(function(){return _1Bb(E(_1Bg)[1],E(_1Bh)[1],E(_1Bi)[1]);});},_1Bj=function(_1Bk,_1Bl){return new F(function(){return _MD(E(_1Bk)[1],E(_1Bl)[1]);});},_1Bm=function(_1Bn){return E(_1Bn);},_1Bo=new T(function(){return B(unCStr("Prelude.Enum.pred{Int}: tried to take `pred\' of minBound"));}),_1Bp=new T(function(){return B(err(_1Bo));}),_1Bq=function(_1Br){var _1Bs=E(E(_1Br)[1]);return _1Bs==(-2147483648)?E(_1Bp):[0,_1Bs-1|0];},_1Bt=new T(function(){return B(unCStr("Prelude.Enum.succ{Int}: tried to take `succ\' of maxBound"));}),_1Bu=new T(function(){return B(err(_1Bt));}),_1Bv=function(_1Bw){var _1Bx=E(E(_1Bw)[1]);return _1Bx==2147483647?E(_1Bu):[0,_1Bx+1|0];},_1By=[0,_1Bv,_1Bq,_1Bm,_1Bm,_1AP,_1B8,_1Bj,_1Bf],_1Bz=function(_1BA,_1BB){var _1BC=E(_1BB);switch(_1BC){case -1:var _1BD=E(_1BA);return _1BD==(-2147483648)?E(_Wv):B(_ZT(_1BD,-1));case 0:return E(_IO);default:return new F(function(){return _ZT(_1BA,_1BC);});}},_1BE=function(_1BF,_1BG){return [0,B(_1Bz(E(_1BF)[1],E(_1BG)[1]))];},_1BH=[0,0],_1BI=[0,_Wv,_1BH],_1BJ=function(_1BK,_1BL){var _1BM=E(_1BK)[1],_1BN=E(E(_1BL)[1]);switch(_1BN){case -1:var _1BO=E(_1BM);if(_1BO==(-2147483648)){return E(_1BI);}else{if(_1BO<=0){if(_1BO>=0){var _1BP=quotRemI(_1BO,-1);return [0,[0,_1BP[1]],[0,_1BP[2]]];}else{var _1BQ=quotRemI(_1BO,-1);return [0,[0,_1BQ[1]],[0,_1BQ[2]]];}}else{var _1BR=quotRemI(_1BO-1|0,-1);return [0,[0,_1BR[1]-1|0],[0,(_1BR[2]+(-1)|0)+1|0]];}}break;case 0:return E(_IO);default:if(_1BM<=0){if(_1BM>=0){var _1BS=quotRemI(_1BM,_1BN);return [0,[0,_1BS[1]],[0,_1BS[2]]];}else{if(_1BN<=0){var _1BT=quotRemI(_1BM,_1BN);return [0,[0,_1BT[1]],[0,_1BT[2]]];}else{var _1BU=quotRemI(_1BM+1|0,_1BN);return [0,[0,_1BU[1]-1|0],[0,(_1BU[2]+_1BN|0)-1|0]];}}}else{if(_1BN>=0){if(_1BM>=0){var _1BV=quotRemI(_1BM,_1BN);return [0,[0,_1BV[1]],[0,_1BV[2]]];}else{if(_1BN<=0){var _1BW=quotRemI(_1BM,_1BN);return [0,[0,_1BW[1]],[0,_1BW[2]]];}else{var _1BX=quotRemI(_1BM+1|0,_1BN);return [0,[0,_1BX[1]-1|0],[0,(_1BX[2]+_1BN|0)-1|0]];}}}else{var _1BY=quotRemI(_1BM-1|0,_1BN);return [0,[0,_1BY[1]-1|0],[0,(_1BY[2]+_1BN|0)+1|0]];}}}},_1BZ=function(_1C0,_1C1){var _1C2=E(E(_1C1)[1]);switch(_1C2){case -1:return E(_1BH);case 0:return E(_IO);default:return [0,B(_TD(E(_1C0)[1],_1C2))];}},_1C3=function(_1C4,_1C5){var _1C6=E(_1C4)[1],_1C7=E(E(_1C5)[1]);switch(_1C7){case -1:var _1C8=E(_1C6);return _1C8==(-2147483648)?E(_Wv):[0,quot(_1C8,-1)];case 0:return E(_IO);default:return [0,quot(_1C6,_1C7)];}},_1C9=function(_1Ca,_1Cb){var _1Cc=E(_1Ca)[1],_1Cd=E(E(_1Cb)[1]);switch(_1Cd){case -1:var _1Ce=E(_1Cc);if(_1Ce==(-2147483648)){return E(_1BI);}else{var _1Cf=quotRemI(_1Ce,-1);return [0,[0,_1Cf[1]],[0,_1Cf[2]]];}break;case 0:return E(_IO);default:var _1Cg=quotRemI(_1Cc,_1Cd);return [0,[0,_1Cg[1]],[0,_1Cg[2]]];}},_1Ch=function(_1Ci,_1Cj){var _1Ck=E(E(_1Cj)[1]);switch(_1Ck){case -1:return E(_1BH);case 0:return E(_IO);default:return [0,E(_1Ci)[1]%_1Ck];}},_1Cl=function(_1Cm){return new F(function(){return _q9(E(_1Cm)[1]);});},_1Cn=function(_1Co){return [0,E(B(_q9(E(_1Co)[1]))),E(_Jq)];},_1Cp=function(_1Cq,_1Cr){return [0,imul(E(_1Cq)[1],E(_1Cr)[1])|0];},_1Cs=function(_1Ct,_1Cu){return [0,E(_1Ct)[1]+E(_1Cu)[1]|0];},_1Cv=function(_1Cw,_1Cx){return [0,E(_1Cw)[1]-E(_1Cx)[1]|0];},_1Cy=function(_1Cz){var _1CA=E(_1Cz),_1CB=_1CA[1];return _1CB<0?[0, -_1CB]:E(_1CA);},_1CC=function(_1CD){return [0,B(_ro(_1CD))];},_1CE=function(_1CF){return [0, -E(_1CF)[1]];},_1CG=[0,-1],_1CH=[0,0],_1CI=[0,1],_1CJ=function(_1CK){var _1CL=E(_1CK)[1];return _1CL>=0?E(_1CL)==0?E(_1CH):E(_1CI):E(_1CG);},_1CM=[0,_1Cs,_1Cp,_1Cv,_1CE,_1Cy,_1CJ,_1CC],_1CN=function(_1CO,_1CP){var _1CQ=E(_1CO),_1CR=E(_1CP);return _1CQ[1]>_1CR[1]?E(_1CQ):E(_1CR);},_1CS=function(_1CT,_1CU){var _1CV=E(_1CT),_1CW=E(_1CU);return _1CV[1]>_1CW[1]?E(_1CW):E(_1CV);},_1CX=function(_1CY,_1CZ){return _1CY>=_1CZ?_1CY!=_1CZ?2:1:0;},_1D0=function(_1D1,_1D2){return new F(function(){return _1CX(E(_1D1)[1],E(_1D2)[1]);});},_1D3=function(_1D4,_1D5){return E(_1D4)[1]>=E(_1D5)[1];},_1D6=function(_1D7,_1D8){return E(_1D7)[1]>E(_1D8)[1];},_1D9=function(_1Da,_1Db){return E(_1Da)[1]<=E(_1Db)[1];},_1Dc=function(_1Dd,_1De){return E(_1Dd)[1]<E(_1De)[1];},_1Df=[0,_RP,_1D0,_1Dc,_1D3,_1D6,_1D9,_1CN,_1CS],_1Dg=[0,_1CM,_1Df,_1Cn],_1Dh=[0,_1Dg,_1By,_1C3,_1Ch,_1BE,_1BZ,_1C9,_1BJ,_1Cl],_1Di=function(_1Dj,_1Dk,_1Dl){while(1){if(!(_1Dk%2)){var _1Dm=B(_qb(_1Dj,_1Dj)),_1Dn=quot(_1Dk,2);_1Dj=_1Dm;_1Dk=_1Dn;continue;}else{var _1Do=E(_1Dk);if(_1Do==1){return new F(function(){return _qb(_1Dj,_1Dl);});}else{var _1Dm=B(_qb(_1Dj,_1Dj));_1Dk=quot(_1Do-1|0,2);var _1Dp=B(_qb(_1Dj,_1Dl));_1Dj=_1Dm;_1Dl=_1Dp;continue;}}}},_1Dq=function(_1Dr,_1Ds){while(1){if(!(_1Ds%2)){var _1Dt=B(_qb(_1Dr,_1Dr)),_1Du=quot(_1Ds,2);_1Dr=_1Dt;_1Ds=_1Du;continue;}else{var _1Dv=E(_1Ds);if(_1Dv==1){return E(_1Dr);}else{return new F(function(){return _1Di(B(_qb(_1Dr,_1Dr)),quot(_1Dv-1|0,2),_1Dr);});}}}},_1Dw=function(_1Dx){return E(E(_1Dx)[1]);},_1Dy=function(_1Dz){return E(E(_1Dz)[2]);},_1DA=function(_1DB,_1DC,_1DD,_1DE,_1DF){return new F(function(){return A(E(E(_1DC)[1])[1],[new T(function(){return B(A(_1DE,[_1DF,new T(function(){return B(A(_Y8,[_1DB,_Jt]));})]));}),new T(function(){return B(A(_Y8,[_1DB,_IP]));})]);});},_1DG=new T(function(){return B(err(_Jr));}),_1DH=function(_1DI,_1DJ,_1DK,_1DL){var _1DM=B(_11i(_1DJ)),_1DN=_1DM[1],_1DO=E(_1DM[2]);if(!B(A(_1DO[3],[_1DL,new T(function(){return B(A(_Y8,[_1DN,_IP]));})]))){if(!B(A(E(_1DO[1])[1],[_1DL,new T(function(){return B(A(_Y8,[_1DN,_IP]));})]))){var _1DP=B(_11i(_1DJ)),_1DQ=_1DP[1],_1DR=new T(function(){return B(_11i(_1DJ));}),_1DS=new T(function(){return B(_11k(_1DR));});return new F(function(){return (function(_1DT,_1DU){while(1){var _1DV=(function(_1DW,_1DX){var _1DY=E(_1DJ),_1DZ=_1DY[3],_1E0=E(_1DY[1]);if(!B(_1DA(_1E0[1],_1E0[2],_1E0[3],_1DY[4],_1DX))){return !B(A(E(E(_1DP[2])[1])[1],[_1DX,new T(function(){return B(A(_Y8,[_1DQ,_Jq]));})]))?B((function(_1E1,_1E2,_1E3){while(1){var _1E4=(function(_1E5,_1E6,_1E7){var _1E8=E(_1DJ),_1E9=_1E8[3],_1Ea=E(_1E8[1]);if(!B(_1DA(_1Ea[1],_1Ea[2],_1Ea[3],_1E8[4],_1E6))){if(!B(A(new T(function(){return B(_qQ(new T(function(){return B(_1Dw(new T(function(){return B(_1Dy(_1DR));},1)));})));}),[_1E6,new T(function(){return B(A(_Y8,[_1DS,_Jq]));})]))){_1E1=new T(function(){return B(A(new T(function(){return B(_1iP(_1DI));}),[_1E5,_1E5]));});_1E2=new T(function(){return B(A(_1E9,[new T(function(){return B(A(new T(function(){return B(_1iT(_1DS));}),[_1E6,new T(function(){return B(A(_Y8,[_1DS,_Jq]));})]));}),new T(function(){return B(A(_Y8,[_1DS,_Jt]));})]));});_1E3=new T(function(){return B(A(new T(function(){return B(_1iP(_1DI));}),[_1E5,_1E7]));});return null;}else{return new F(function(){return A(new T(function(){return B(_1iP(_1DI));}),[_1E5,_1E7]);});}}else{_1E1=new T(function(){return B(A(new T(function(){return B(_1iP(_1DI));}),[_1E5,_1E5]));});_1E2=new T(function(){return B(A(_1E9,[_1E6,new T(function(){return B(A(_Y8,[_1DS,_Jt]));})]));});var _1Eb=_1E7;_1E3=_1Eb;return null;}})(_1E1,_1E2,_1E3);if(_1E4!=null){return _1E4;}}})(new T(function(){return B(A(new T(function(){return B(_1iP(_1DI));}),[_1DW,_1DW]));}),new T(function(){return B(A(_1DZ,[new T(function(){return B(A(new T(function(){return B(_1iT(_1DQ));}),[_1DX,new T(function(){return B(A(_Y8,[_1DQ,_Jq]));})]));}),new T(function(){return B(A(_Y8,[_1DQ,_Jt]));})]));}),_1DW)):E(_1DW);}else{_1DT=new T(function(){return B(A(new T(function(){return B(_1iP(_1DI));}),[_1DW,_1DW]));});_1DU=new T(function(){return B(A(_1DZ,[_1DX,new T(function(){return B(A(_Y8,[_1DQ,_Jt]));})]));});return null;}})(_1DT,_1DU);if(_1DV!=null){return _1DV;}}})(_1DK,_1DL);});}else{return new F(function(){return A(_Y8,[_1DI,_Jq]);});}}else{return E(_1DG);}},_1Ec=function(_1Ed,_1Ee){var _1Ef=E(_1Ed);return _1Ef[0]==0?_1Ef[1]*Math.pow(2,_1Ee):I_toNumber(_1Ef[1])*Math.pow(2,_1Ee);},_1Eg=function(_1Eh,_1Ei){var _1Ej=B(_1k0(_1Ei)),_1Ek=_1Ej[1],_1El=_1Ej[2],_1Em=new T(function(){return B(_11k(new T(function(){return B(_11i(_1Eh));},1)));});if(_1El<0){var _1En= -_1El;if(_1En>=0){var _1Eo=E(_1En),_1Ep=_1Eo==0?E(_Jq):B(_1Dq(_1A4,_1Eo));if(!B(_HL(_1Ep,_HF))){var _1Eq=B(_10q(_1Ek,_1Ep));return [0,new T(function(){return B(A(_Y8,[_1Em,_1Eq[1]]));}),new T(function(){return [0,B(_1Ec(_1Eq[2],_1El))];})];}else{return E(_IO);}}else{return E(_Js);}}else{return [0,new T(function(){return B(A(_1iP,[_1Em,new T(function(){return B(A(_Y8,[_1Em,_1Ek]));}),new T(function(){return B(_1DH(_1Em,_1Dh,new T(function(){return B(A(_Y8,[_1Em,_1A4]));}),[0,_1El]));})]));}),_Up];}},_1Er=function(_1Es,_1Et){var _1Eu=B(_1Eg(_1Es,E(_1Et)[1])),_1Ev=_1Eu[1];if(E(_1Eu[2])[1]<=0){return E(_1Ev);}else{var _1Ew=E(B(_11i(_1Es))[1]);return new F(function(){return A(_1Ew[1],[_1Ev,new T(function(){return B(A(_1Ew[7],[_1jZ]));})]);});}},_1Ex=function(_1Ey,_1Ez){var _1EA=B(_1Eg(_1Ey,E(_1Ez)[1])),_1EB=_1EA[1];if(E(_1EA[2])[1]>=0){return E(_1EB);}else{var _1EC=E(B(_11i(_1Ey))[1]);return new F(function(){return A(_1EC[3],[_1EB,new T(function(){return B(A(_1EC[7],[_1jZ]));})]);});}},_1ED=function(_1EE,_1EF){var _1EG=B(_1Eg(_1EE,E(_1EF)[1]));return [0,_1EG[1],_1EG[2]];},_1EH=function(_1EI,_1EJ){var _1EK=B(_1Eg(_1EI,_1EJ)),_1EL=_1EK[1],_1EM=E(_1EK[2])[1],_1EN=new T(function(){var _1EO=E(B(_11i(_1EI))[1]),_1EP=_1EO[7];return _1EM>=0?B(A(_1EO[1],[_1EL,new T(function(){return B(A(_1EP,[_1jZ]));})])):B(A(_1EO[3],[_1EL,new T(function(){return B(A(_1EP,[_1jZ]));})]));},1);if(_1EM<0){var _1EQ= -_1EM-0.5;if(_1EQ>=0){if(!E(_1EQ)){var _1ER=E(_1EI),_1ES=E(_1ER[1]);return !B(_1DA(_1ES[1],_1ES[2],_1ES[3],_1ER[4],_1EL))?E(_1EN):E(_1EL);}else{return E(_1EN);}}else{return E(_1EL);}}else{var _1ET=_1EM-0.5;if(_1ET>=0){if(!E(_1ET)){var _1EU=E(_1EI),_1EV=E(_1EU[1]);return !B(_1DA(_1EV[1],_1EV[2],_1EV[3],_1EU[4],_1EL))?E(_1EN):E(_1EL);}else{return E(_1EN);}}else{return E(_1EL);}}},_1EW=function(_1EX,_1EY){return new F(function(){return _1EH(_1EX,E(_1EY)[1]);});},_1EZ=function(_1F0,_1F1){return E(B(_1Eg(_1F0,E(_1F1)[1]))[1]);},_1F2=[0,_1AO,_UK,_1ED,_1EZ,_1EW,_1Er,_1Ex],_1F3=function(_1F4,_1F5){return E(_1F4)[1]!=E(_1F5)[1]?true:false;},_1F6=function(_1F7,_1F8){return E(_1F7)[1]==E(_1F8)[1];},_1F9=[0,_1F6,_1F3],_1Fa=function(_1Fb,_1Fc){return E(_1Fb)[1]<E(_1Fc)[1];},_1Fd=function(_1Fe,_1Ff){return E(_1Fe)[1]<=E(_1Ff)[1];},_1Fg=function(_1Fh,_1Fi){return E(_1Fh)[1]>E(_1Fi)[1];},_1Fj=function(_1Fk,_1Fl){return E(_1Fk)[1]>=E(_1Fl)[1];},_1Fm=function(_1Fn,_1Fo){var _1Fp=E(_1Fn)[1],_1Fq=E(_1Fo)[1];return _1Fp>=_1Fq?_1Fp!=_1Fq?2:1:0;},_1Fr=function(_1Fs,_1Ft){var _1Fu=E(_1Fs),_1Fv=E(_1Ft);return _1Fu[1]>_1Fv[1]?E(_1Fu):E(_1Fv);},_1Fw=function(_1Fx,_1Fy){var _1Fz=E(_1Fx),_1FA=E(_1Fy);return _1Fz[1]>_1FA[1]?E(_1FA):E(_1Fz);},_1FB=[0,_1F9,_1Fm,_1Fa,_1Fj,_1Fg,_1Fd,_1Fr,_1Fw],_1FC=function(_1FD,_1FE){while(1){var _1FF=(function(_1FG,_1FH){var _1FI=E(_1k3)[1]["v"]["i8"][(255&_1FG>>>0)>>>0&4294967295];if(_1FH>_1FI){if(_1FI>=8){var _1FJ=_1FG>>8,_1FK=_1FH-8|0;_1FD=_1FJ;_1FE=_1FK;return null;}else{return [0,new T(function(){return B(_q9(_1FG>>_1FI));}),_1FH-_1FI|0];}}else{return [0,new T(function(){return B(_q9(_1FG>>_1FH));}),0];}})(_1FD,_1FE);if(_1FF!=null){return _1FF;}}},_1FL=function(_1FM){var _1FN=decodeFloat(_1FM),_1FO=_1FN[1],_1FP=_1FN[2];if(_1FP<0){var _1FQ=function(_1FR){if(!_1FR){return [0,B(_q9(_1FO)),B(_1kz(_1jZ, -_1FP))];}else{var _1FS=B(_1FC(_1FO, -_1FP));return [0,E(_1FS[1]),B(_1kz(_1jZ,_1FS[2]))];}};return (_1FO>>>0&1)>>>0==0?B(_1FQ(1)):B(_1FQ(0));}else{return [0,B(_1kz(B(_q9(_1FO)),_1FP)),_1jZ];}},_1FT=function(_1FU){var _1FV=B(_1FL(E(_1FU)[1]));return [0,E(_1FV[1]),E(_1FV[2])];},_1FW=[0,_1zr,_1FB,_1FT],_1FX=[0,-1],_1FY=[0,1],_1FZ=function(_1G0,_1G1){var _1G2=E(_1G0);return _1G2[0]==0?_1G2[1]*Math.pow(2,_1G1):I_toNumber(_1G2[1])*Math.pow(2,_1G1);},_1G3=function(_1G4,_1G5){var _1G6=decodeFloat(_1G5),_1G7=_1G6[1],_1G8=_1G6[2],_1G9=new T(function(){return B(_11k(new T(function(){return B(_11i(_1G4));},1)));});if(_1G8<0){var _1Ga=new T(function(){if(_1G7<0){var _1Gb= -_1G8;if(_1Gb<32){var _1Gc=[0, -( -_1G7>>_1Gb)];}else{var _1Gc= -_1G7>=0?E(_LF):E(_1FY);}var _1Gd=_1Gc,_1Ge=_1Gd,_1Gf=_1Ge;}else{var _1Gg= -_1G8;if(_1Gg<32){var _1Gh=[0,_1G7>>_1Gg];}else{var _1Gh=_1G7>=0?E(_LF):E(_1FX);}var _1Gi=_1Gh,_1Gj=_1Gi,_1Gf=_1Gj;}var _1Gk=_1Gf;return _1Gk;});return [0,new T(function(){return B(A(_Y8,[_1G9,new T(function(){return B(_q9(E(_1Ga)[1]));})]));}),new T(function(){var _1Gl= -_1G8;if(_1Gl<32){var _1Gm=[0,B(_1FZ(B(_q9(_1G7-(E(_1Ga)[1]<<_1Gl)|0)),_1G8))];}else{var _1Gm=[0,B(_1FZ(B(_q9(_1G7)),_1G8))];}var _1Gn=_1Gm,_1Go=_1Gn,_1Gp=_1Go;return _1Gp;})];}else{return [0,new T(function(){return B(A(_1iP,[_1G9,new T(function(){return B(A(_Y8,[_1G9,new T(function(){return B(_q9(_1G7));})]));}),new T(function(){return B(_1DH(_1G9,_1Dh,new T(function(){return B(A(_Y8,[_1G9,_1A4]));}),[0,_1G8]));})]));}),_1za];}},_1Gq=function(_1Gr,_1Gs){var _1Gt=B(_1G3(_1Gr,E(_1Gs)[1])),_1Gu=_1Gt[1];if(E(_1Gt[2])[1]<=0){return E(_1Gu);}else{var _1Gv=E(B(_11i(_1Gr))[1]);return new F(function(){return A(_1Gv[1],[_1Gu,new T(function(){return B(A(_1Gv[7],[_1jZ]));})]);});}},_1Gw=function(_1Gx,_1Gy){var _1Gz=B(_1G3(_1Gx,E(_1Gy)[1])),_1GA=_1Gz[1];if(E(_1Gz[2])[1]>=0){return E(_1GA);}else{var _1GB=E(B(_11i(_1Gx))[1]);return new F(function(){return A(_1GB[3],[_1GA,new T(function(){return B(A(_1GB[7],[_1jZ]));})]);});}},_1GC=function(_1GD,_1GE){var _1GF=B(_1G3(_1GD,E(_1GE)[1]));return [0,_1GF[1],_1GF[2]];},_1GG=function(_1GH,_1GI){var _1GJ=B(_1G3(_1GH,_1GI)),_1GK=_1GJ[1],_1GL=E(_1GJ[2])[1],_1GM=new T(function(){var _1GN=E(B(_11i(_1GH))[1]),_1GO=_1GN[7];return _1GL>=0?B(A(_1GN[1],[_1GK,new T(function(){return B(A(_1GO,[_1jZ]));})])):B(A(_1GN[3],[_1GK,new T(function(){return B(A(_1GO,[_1jZ]));})]));},1);if(_1GL<0){var _1GP= -_1GL-0.5;if(_1GP>=0){if(!E(_1GP)){var _1GQ=E(_1GH),_1GR=E(_1GQ[1]);return !B(_1DA(_1GR[1],_1GR[2],_1GR[3],_1GQ[4],_1GK))?E(_1GM):E(_1GK);}else{return E(_1GM);}}else{return E(_1GK);}}else{var _1GS=_1GL-0.5;if(_1GS>=0){if(!E(_1GS)){var _1GT=E(_1GH),_1GU=E(_1GT[1]);return !B(_1DA(_1GU[1],_1GU[2],_1GU[3],_1GT[4],_1GK))?E(_1GM):E(_1GK);}else{return E(_1GM);}}else{return E(_1GK);}}},_1GV=function(_1GW,_1GX){return new F(function(){return _1GG(_1GW,E(_1GX)[1]);});},_1GY=function(_1GZ,_1H0){return E(B(_1G3(_1GZ,E(_1H0)[1]))[1]);},_1H1=[0,_1FW,_1zv,_1GC,_1GY,_1GV,_1Gq,_1Gw],_1H2=function(_1H3,_1H4,_1H5){while(1){if(!(_1H4%2)){var _1H6=_1H3*_1H3,_1H7=quot(_1H4,2);_1H3=_1H6;_1H4=_1H7;continue;}else{var _1H8=E(_1H4);if(_1H8==1){return _1H3*_1H5;}else{var _1H6=_1H3*_1H3;_1H4=quot(_1H8-1|0,2);var _1H9=_1H3*_1H5;_1H3=_1H6;_1H5=_1H9;continue;}}}},_1Ha=function(_1Hb,_1Hc){while(1){if(!(_1Hc%2)){var _1Hd=_1Hb*_1Hb,_1He=quot(_1Hc,2);_1Hb=_1Hd;_1Hc=_1He;continue;}else{var _1Hf=E(_1Hc);if(_1Hf==1){return E(_1Hb);}else{return new F(function(){return _1H2(_1Hb*_1Hb,quot(_1Hf-1|0,2),_1Hb);});}}}},_1Hg=function(_1Hh){return new F(function(){return err(B(unAppCStr("Char.intToDigit: not a digit ",new T(function(){if(_1Hh>=0){var _1Hi=jsShowI(_1Hh),_1Hj=_1Hi,_1Hk=fromJSStr(_1Hj);}else{var _1Hl=jsShowI(_1Hh),_1Hm=_1Hl,_1Hk=fromJSStr(_1Hm);}var _1Hn=_1Hk;return _1Hn;}))));});},_1Ho=function(_1Hp){var _1Hq=function(_1Hr){if(_1Hp<10){return new F(function(){return _1Hg(_1Hp);});}else{if(_1Hp>15){return new F(function(){return _1Hg(_1Hp);});}else{return (97+_1Hp|0)-10|0;}}};if(_1Hp<0){return new F(function(){return _1Hq(_);});}else{if(_1Hp>9){return new F(function(){return _1Hq(_);});}else{return 48+_1Hp|0;}}},_1Hs=function(_1Ht){return [0,B(_1Ho(E(_1Ht)[1]))];},_1Hu=new T(function(){return B(_Sp("GHC/Float.lhs:619:11-64|d : ds\'"));}),_1Hv=function(_1Hw,_1Hx){if(E(_1Hw)[1]<=0){var _1Hy=B(_8X(_1Hs,[1,_LF,_1Hx]));return _1Hy[0]==0?E(_1Hu):[0,_1Hy[1],_1Hy[2]];}else{var _1Hz=B(_8X(_1Hs,_1Hx));return _1Hz[0]==0?E(_1Hu):[0,_1Hz[1],_1Hz[2]];}},_1HA=function(_1HB){return E(E(_1HB)[1]);},_1HC=function(_1HD){return E(E(_1HD)[1]);},_1HE=function(_1HF){return E(E(_1HF)[1]);},_1HG=[0,48],_1HH=[1,_1HG,_1g],_1HI=[0,46],_1HJ=function(_1HK,_1HL,_1HM){while(1){var _1HN=(function(_1HO,_1HP,_1HQ){var _1HR=E(_1HO);if(!_1HR){var _1HS=B(_1xT(_1HP,_1g));return _1HS[0]==0?[1,_1HG,[1,_1HI,new T(function(){var _1HT=E(_1HQ);return _1HT[0]==0?E(_1HH):E(_1HT);})]]:B(_5B(_1HS,[1,_1HI,new T(function(){var _1HU=E(_1HQ);return _1HU[0]==0?E(_1HH):E(_1HU);})]));}else{var _1HV=E(_1HQ);if(!_1HV[0]){_1HK=_1HR-1|0;var _1HW=[1,_1HG,_1HP];_1HM=_1g;_1HL=_1HW;return null;}else{_1HK=_1HR-1|0;var _1HW=[1,_1HV[1],_1HP];_1HM=_1HV[2];_1HL=_1HW;return null;}}})(_1HK,_1HL,_1HM);if(_1HN!=null){return _1HN;}}},_1HX=[0,0],_1HY=function(_1HZ,_1I0,_1I1){return new F(function(){return A(_1HZ,[[1,_do,new T(function(){return B(A(_1I0,[_1I1]));})]]);});},_1I2=new T(function(){return B(unCStr("foldr1"));}),_1I3=new T(function(){return B(_ai(_1I2));}),_1I4=function(_1I5,_1I6){var _1I7=E(_1I6);if(!_1I7[0]){return E(_1I3);}else{var _1I8=_1I7[1],_1I9=E(_1I7[2]);if(!_1I9[0]){return E(_1I8);}else{return new F(function(){return A(_1I5,[_1I8,new T(function(){return B(_1I4(_1I5,_1I9));})]);});}}},_1Ia=new T(function(){return B(unCStr(" out of range "));}),_1Ib=new T(function(){return B(unCStr("}.index: Index "));}),_1Ic=new T(function(){return B(unCStr("Ix{"));}),_1Id=[1,_5K,_1g],_1Ie=[1,_5K,_1Id],_1If=function(_1Ig,_1Ih,_1Ii,_1Ij,_1Ik){return new F(function(){return err(B(_5B(_1Ic,new T(function(){return B(_5B(_1Ig,new T(function(){return B(_5B(_1Ib,[1,_5L,new T(function(){return B(A(_1Ik,[_1HX,_1Ih,[1,_5K,new T(function(){return B(_5B(_1Ia,[1,_5L,[1,_5L,new T(function(){return B(A(_1I4,[_1HY,[1,new T(function(){return B(A(_1Ik,[_Au,_1Ii]));}),[1,new T(function(){return B(A(_1Ik,[_Au,_1Ij]));}),_1g]],_1Ie]));})]]));})]]));})]));},1)));},1))));});},_1Il=function(_1Im,_1In,_1Io,_1Ip){var _1Iq=E(_1Io);return new F(function(){return _1If(_1Im,_1In,_1Iq[1],_1Iq[2],E(_1Ip)[1]);});},_1Ir=function(_1Is,_1It,_1Iu,_1Iv){return new F(function(){return _1Il(_1Iv,_1Iu,_1It,_1Is);});},_1Iw=new T(function(){return B(unCStr("Int"));}),_1Ix=function(_1Iy,_1Iz,_1IA){return new F(function(){return _1Ir(_yH,[0,_1Iz,_1IA],_1Iy,_1Iw);});},_1IB=new T(function(){return B(unCStr("(Array.!): undefined array element"));}),_1IC=new T(function(){return B(err(_1IB));}),_1ID=[0,1100],_1IE=[0,_LF,_1ID],_1IF=function(_1IG){return new F(function(){return _1Ir(_yH,_1IE,[0,_1IG],_1Iw);});},_1IH=function(_){var _1II=newArr(1101,_1IC),_1IJ=_1II;return new F(function(){return (function(_1IK,_){while(1){var _1IL=(function(_1IM,_){if(0>_1IM){return new F(function(){return _1IF(_1IM);});}else{if(_1IM>1100){return new F(function(){return _1IF(_1IM);});}else{var _=_1IJ[_1IM]=new T(function(){if(_1IM>=0){var _1IN=E(_1IM),_1IO=_1IN==0?E(_Jq):B(_1Dq(_1A4,_1IN));}else{var _1IO=E(_Js);}var _1IP=_1IO;return _1IP;}),_1IQ=E(_1IM);if(_1IQ==1100){var _1IR=_1IJ,_1IS=_1IR;return [0,E(_LF),E(_1ID),1101,_1IS];}else{_1IK=_1IQ+1|0;return null;}}}})(_1IK,_);if(_1IL!=null){return _1IL;}}})(0,_);});},_1IT=function(_1IU){var _1IV=B(A(_1IU,[_])),_1IW=_1IV;return E(_1IW);},_1IX=new T(function(){return B(_1IT(_1IH));}),_1IY=[0,10],_1IZ=[0,324],_1J0=[0,_LF,_1IZ],_1J1=function(_1J2){return new F(function(){return _1Ir(_yH,_1J0,[0,_1J2],_1Iw);});},_1J3=function(_){var _1J4=newArr(325,_1IC),_1J5=_1J4;return new F(function(){return (function(_1J6,_){while(1){var _1J7=(function(_1J8,_){if(0>_1J8){return new F(function(){return _1J1(_1J8);});}else{if(_1J8>324){return new F(function(){return _1J1(_1J8);});}else{var _=_1J5[_1J8]=new T(function(){if(_1J8>=0){var _1J9=E(_1J8),_1Ja=_1J9==0?E(_Jq):B(_1Dq(_1IY,_1J9));}else{var _1Ja=E(_Js);}var _1Jb=_1Ja;return _1Jb;}),_1Jc=E(_1J8);if(_1Jc==324){var _1Jd=_1J5,_1Je=_1Jd;return [0,E(_LF),E(_1IZ),325,_1Je];}else{_1J6=_1Jc+1|0;return null;}}}})(_1J6,_);if(_1J7!=null){return _1J7;}}})(0,_);});},_1Jf=new T(function(){return B(_1IT(_1J3));}),_1Jg=function(_1Jh,_1Ji){var _1Jj=[0,_1Ji],_1Jk=function(_1Jl){if(!B(_HL(_1Jh,_1IY))){if(_1Ji>=0){var _1Jm=E(_1Ji);return _1Jm==0?E(_Jq):B(_1Dq(_1Jh,_1Jm));}else{return E(_Js);}}else{if(_1Ji>324){if(_1Ji>=0){var _1Jn=E(_1Ji);return _1Jn==0?E(_Jq):B(_1Dq(_1Jh,_1Jn));}else{return E(_Js);}}else{var _1Jo=E(_1Jf),_1Jp=E(_1Jo[1]),_1Jq=_1Jp[1],_1Jr=E(_1Jo[2]);if(_1Jq>_1Ji){return new F(function(){return _1Ix(_1Jj,_1Jp,_1Jr);});}else{if(_1Ji>_1Jr[1]){return new F(function(){return _1Ix(_1Jj,_1Jp,_1Jr);});}else{return E(_1Jo[4][_1Ji-_1Jq|0]);}}}}};if(!B(_HL(_1Jh,_1A4))){return new F(function(){return _1Jk(_);});}else{if(_1Ji<0){return new F(function(){return _1Jk(_);});}else{if(_1Ji>1100){return new F(function(){return _1Jk(_);});}else{var _1Js=E(_1IX),_1Jt=E(_1Js[1]),_1Ju=_1Jt[1],_1Jv=E(_1Js[2]);if(_1Ju>_1Ji){return new F(function(){return _1Ix(_1Jj,_1Jt,_1Jv);});}else{if(_1Ji>_1Jv[1]){return new F(function(){return _1Ix(_1Jj,_1Jt,_1Jv);});}else{return E(_1Js[4][_1Ji-_1Ju|0]);}}}}}},_1Jw=[1,_LF,_1g],_1Jx=function(_1Jy,_1Jz,_1JA,_1JB,_1JC,_1JD,_1JE,_1JF){if(!B(A(_1Jy,[_1JF,new T(function(){return B(A(_Y8,[B(_1HC(B(_1HA(_1Jz)))),_HF]));})]))){var _1JG=new T(function(){return B(A(_1JA,[_1JF]));}),_1JH=new T(function(){return B(A(_1JB,[_1JF]));}),_1JI=new T(function(){return [0,E(B(A(_1JC,[_1JF]))[1])[1]-E(_1JH)[1]|0];}),_1JJ=new T(function(){return B(A(_1JD,[_1JF]));}),_1JK=new T(function(){return E(E(_1JJ)[2]);}),_1JL=new T(function(){var _1JM=E(_1JK),_1JN=_1JM[1],_1JO=E(_1JI)[1]-_1JN|0;if(_1JO<=0){var _1JP=[0,new T(function(){return E(E(_1JJ)[1]);}),_1JM];}else{var _1JP=[0,new T(function(){var _1JQ=B(_1Jg(_1JG,_1JO));if(!B(_HL(_1JQ,_HF))){var _1JR=B(_Jb(E(_1JJ)[1],_1JQ));}else{var _1JR=E(_IO);}var _1JS=_1JR;return _1JS;}),[0,_1JN+_1JO|0]];}var _1JT=_1JP,_1JU=_1JT,_1JV=_1JU,_1JW=_1JV;return _1JW;}),_1JX=new T(function(){return E(E(_1JL)[2]);}),_1JY=new T(function(){return E(E(_1JL)[1]);}),_1JZ=new T(function(){var _1K0=E(_1JX)[1];if(_1K0<0){if(_1K0<=E(_1JI)[1]){var _1K1=[0,new T(function(){return B(_qb(_1JY,_1A4));}),new T(function(){return B(_qb(B(_1Jg(_1JG, -_1K0)),_1A4));}),_1jZ,_1jZ];}else{var _1K1=!B(_HL(_1JY,B(_1Jg(_1JG,E(_1JH)[1]-1|0))))?[0,new T(function(){return B(_qb(_1JY,_1A4));}),new T(function(){return B(_qb(B(_1Jg(_1JG, -_1K0)),_1A4));}),_1jZ,_1jZ]:[0,new T(function(){return B(_qb(B(_qb(_1JY,_1JG)),_1A4));}),new T(function(){return B(_qb(B(_1Jg(_1JG, -_1K0+1|0)),_1A4));}),_1JG,_1jZ];}var _1K2=_1K1,_1K3=_1K2,_1K4=_1K3;}else{var _1K5=new T(function(){return B(_1Jg(_1JG,_1K0));}),_1K4=!B(_HL(_1JY,B(_1Jg(_1JG,E(_1JH)[1]-1|0))))?[0,new T(function(){return B(_qb(B(_qb(_1JY,_1K5)),_1A4));}),_1A4,_1K5,_1K5]:[0,new T(function(){return B(_qb(B(_qb(B(_qb(_1JY,_1K5)),_1JG)),_1A4));}),new T(function(){return B(_qb(_1A4,_1JG));}),new T(function(){return B(_qb(_1K5,_1JG));}),_1K5];}var _1K6=_1K4,_1K7=_1K6;return _1K7;}),_1K8=new T(function(){return E(E(_1JZ)[2]);}),_1K9=new T(function(){return E(E(_1JZ)[3]);}),_1Ka=new T(function(){return E(E(_1JZ)[1]);}),_1Kb=new T(function(){var _1Kc=new T(function(){return B(_pT(_1Ka,_1K9));}),_1Kd=function(_1Ke){var _1Kf=(Math.log(B(_1z5(B(_pT(_1JY,_1jZ)))))+E(_1JX)[1]*Math.log(B(_1z5(_1JG))))/Math.log(B(_1z5(_1JE))),_1Kg=_1Kf&4294967295;return _1Kg>=_1Kf?E(_1Kg):_1Kg+1|0;},_1Kh=function(_1Ki){while(1){if(_1Ki<0){if(!B(_rr(B(_qb(B(_1Jg(_1JE, -_1Ki)),_1Kc)),_1K8))){var _1Kj=_1Ki+1|0;_1Ki=_1Kj;continue;}else{return E(_1Ki);}}else{if(!B(_rr(_1Kc,B(_qb(B(_1Jg(_1JE,_1Ki)),_1K8))))){var _1Kj=_1Ki+1|0;_1Ki=_1Kj;continue;}else{return E(_1Ki);}}}};if(!B(_HL(_1JG,_1A4))){var _1Kk=[0,B(_1Kh(B(_1Kd(_))))];}else{if(!B(_HL(_1JE,_1IY))){var _1Kl=[0,B(_1Kh(B(_1Kd(_))))];}else{var _1Km=(E(_1JH)[1]-1|0)+E(_1JK)[1]|0;if(_1Km<0){var _1Kn=[0,B(_1Kh(quot(imul(_1Km,8651)|0,28738)))];}else{var _1Kn=[0,B(_1Kh(quot(imul(_1Km,8651)|0,28738)+1|0))];}var _1Ko=_1Kn,_1Kp=_1Ko,_1Kq=_1Kp,_1Kr=_1Kq,_1Ks=_1Kr,_1Kl=_1Ks;}var _1Kk=_1Kl;}return _1Kk;});return [0,new T(function(){var _1Kt=E(_1Kb)[1],_1Ku=function(_1Kv,_1Kw,_1Kx,_1Ky,_1Kz){while(1){var _1KA=(function(_1KB,_1KC,_1KD,_1KE,_1KF){if(!B(_HL(_1KD,_HF))){var _1KG=B(_10q(B(_qb(_1KC,_1JE)),_1KD)),_1KH=_1KG[1],_1KI=_1KG[2],_1KJ=B(_qb(_1KF,_1JE)),_1KK=B(_qb(_1KE,_1JE));if(!B(_HT(_1KI,_1KJ))){if(!B(_Kw(B(_pT(_1KI,_1KK)),_1KD))){var _1KL=[1,_1KH,_1KB];_1Kw=_1KI;var _1KM=_1KD;_1Ky=_1KK;_1Kz=_1KJ;_1Kv=_1KL;_1Kx=_1KM;return null;}else{return [1,new T(function(){return B(_pT(_1KH,_1jZ));}),_1KB];}}else{return !B(_Kw(B(_pT(_1KI,_1KK)),_1KD))?[1,_1KH,_1KB]:!B(_HT(B(_qb(_1KI,_1A4)),_1KD))?[1,new T(function(){return B(_pT(_1KH,_1jZ));}),_1KB]:[1,_1KH,_1KB];}}else{return E(_IO);}})(_1Kv,_1Kw,_1Kx,_1Ky,_1Kz);if(_1KA!=null){return _1KA;}}};if(_1Kt<0){var _1KN=B(_1Jg(_1JE, -_1Kt)),_1KO=B(_8X(_1CC,B(_1xT(B(_1Ku(_1g,B(_qb(_1Ka,_1KN)),_1K8,B(_qb(_1K9,_1KN)),B(_qb(E(_1JZ)[4],_1KN)))),_1g))));}else{var _1KO=B(_8X(_1CC,B(_1xT(B(_1Ku(_1g,_1Ka,B(_qb(_1K8,B(_1Jg(_1JE,_1Kt)))),_1K9,E(_1JZ)[4])),_1g))));}var _1KP=_1KO,_1KQ=_1KP;return _1KQ;}),_1Kb];}else{return [0,_1Jw,_LF];}},_1KR=function(_1KS,_1KT){while(1){var _1KU=E(_1KT);if(!_1KU[0]){return true;}else{if(!B(A(_1KS,[_1KU[1]]))){return false;}else{_1KT=_1KU[2];continue;}}}},_1KV=function(_1KW){return E(_1KW)[1]%2==0?true:false;},_1KX=new T(function(){return B(unCStr("roundTo: bad Value"));}),_1KY=new T(function(){return B(err(_1KX));}),_1KZ=function(_1L0){return E(E(_1L0)[1])==0?true:false;},_1L1=function(_1L2){return _1L2>1?[1,_LF,new T(function(){return B(_1L1(_1L2-1|0));})]:E(_1Jw);},_1L3=function(_1L4,_1L5,_1L6){var _1L7=function(_1L8,_1L9,_1La){var _1Lb=E(_1La);if(!_1Lb[0]){return [0,_LF,new T(function(){var _1Lc=E(_1L8)[1];return _1Lc>0?B(_1L1(_1Lc)):[0];})];}else{var _1Ld=_1Lb[1],_1Le=_1Lb[2],_1Lf=E(E(_1L8)[1]);if(!_1Lf){var _1Lg=E(_1Ld)[1],_1Lh=E(new T(function(){return [0,quot(E(_1L4)[1],2)];}))[1];return _1Lg!=_1Lh?[0,new T(function(){return _1Lg<_1Lh?E(_LF):E(_1FY);}),_1g]:!E(_1L9)?[0,new T(function(){return _1Lg<_1Lh?E(_LF):E(_1FY);}),_1g]:!B(_1KR(_1KZ,_1Le))?[0,new T(function(){return _1Lg<_1Lh?E(_LF):E(_1FY);}),_1g]:[0,_LF,_1g];}else{var _1Li=B(_1L7([0,_1Lf-1|0],new T(function(){return B(_1KV(_1Ld));},1),_1Le)),_1Lj=_1Li[2],_1Lk=E(_1Li[1])[1]+E(_1Ld)[1]|0;return _1Lk!=E(_1L4)[1]?[0,_LF,[1,[0,_1Lk],_1Lj]]:[0,_1FY,[1,_LF,_1Lj]];}}},_1Ll=B(_1L7(_1L5,_fI,_1L6));switch(E(E(_1Ll[1])[1])){case 0:return E(_1Ll);case 1:return [0,_1FY,[1,_1FY,_1Ll[2]]];default:return E(_1KY);}},_1Lm=function(_1Ln){return E(E(_1Ln)[3]);},_1Lo=0,_1Lp=1,_1Lq=[0,10],_1Lr=new T(function(){return B(unCStr("e0"));}),_1Ls=function(_1Lt,_1Lu){var _1Lv=E(_1Lt);if(!_1Lv[0]){return E(_1Lr);}else{var _1Lw=_1Lv[1];return _1Lu>1?[1,_1Lw,new T(function(){return B(_1Ls(_1Lv[2],_1Lu-1|0));})]:[1,_1Lw,_1Lr];}},_1Lx=new T(function(){return B(_Sp("GHC/Float.lhs:591:12-70|(d : ds\')"));}),_1Ly=[0,101],_1Lz=new T(function(){return B(unCStr("Infinity"));}),_1LA=new T(function(){return B(unCStr("-Infinity"));}),_1LB=new T(function(){return B(unCStr("NaN"));}),_1LC=new T(function(){return B(unCStr("formatRealFloat/doFmt/FFExponent: []"));}),_1LD=new T(function(){return B(err(_1LC));}),_1LE=new T(function(){return B(unCStr("0.0e0"));}),_1LF=function(_1LG){return E(E(_1LG)[4]);},_1LH=new T(function(){return [1,_1HG,_1LH];}),_1LI=function(_1LJ,_1LK,_1LL,_1LM,_1LN,_1LO,_1LP,_1LQ,_1LR,_1LS,_1LT,_1LU){if(!B(A(_1LP,[_1LU]))){var _1LV=new T(function(){return B(_1HC(new T(function(){return B(_1HA(_1LK));},1)));});if(!B(A(_1LQ,[_1LU]))){var _1LW=function(_1LX,_1LY,_1LZ){while(1){var _1M0=(function(_1M1,_1M2,_1M3){switch(E(_1M1)){case 0:var _1M4=E(_1LT);if(!_1M4[0]){var _1M5=B(_8X(_1Hs,_1M2));if(!_1M5[0]){return E(_1LD);}else{var _1M6=_1M5[2],_1M7=E(_1M5[1]),_1M8=function(_1M9){var _1Ma=E(_1M6);return _1Ma[0]==0?[1,_1M7,new T(function(){return B(unAppCStr(".0e",new T(function(){return B(_5M(0,E(_1M3)[1]-1|0,_1g));})));})]:[1,_1M7,[1,_1HI,new T(function(){return B(_5B(_1Ma,[1,_1Ly,new T(function(){return B(_5M(0,E(_1M3)[1]-1|0,_1g));})]));})]];};return E(_1M7[1])==48?E(_1M6)[0]==0?E(_1LE):B(_1M8(_)):B(_1M8(_));}}else{var _1Mb=new T(function(){var _1Mc=E(_1M4[1]);return _1Mc[1]>1?E(_1Mc):E(_1FY);},1),_1Md=function(_1Me){var _1Mf=new T(function(){var _1Mg=B(_1L3(_1Lq,new T(function(){return [0,E(_1Mb)[1]+1|0];},1),_1M2));return [0,_1Mg[1],_1Mg[2]];}),_1Mh=new T(function(){return E(E(_1Mf)[1]);}),_1Mi=new T(function(){if(E(_1Mh)[1]<=0){var _1Mj=B(_8X(_1Hs,E(_1Mf)[2])),_1Mk=_1Mj[0]==0?E(_1Lx):[0,_1Mj[1],_1Mj[2]];}else{var _1Ml=E(E(_1Mf)[2]);if(!_1Ml[0]){var _1Mm=E(_1qp);}else{var _1Mn=B(_8X(_1Hs,B(_1qk(_1Ml[1],_1Ml[2])))),_1Mm=_1Mn[0]==0?E(_1Lx):[0,_1Mn[1],_1Mn[2]];}var _1Mo=_1Mm,_1Mk=_1Mo;}var _1Mp=_1Mk,_1Mq=_1Mp;return _1Mq;});return [1,new T(function(){return E(E(_1Mi)[1]);}),[1,_1HI,new T(function(){return B(_5B(E(_1Mi)[2],[1,_1Ly,new T(function(){return B(_5M(0,(E(_1M3)[1]-1|0)+E(_1Mh)[1]|0,_1g));})]));})]];},_1Mr=E(_1M2);if(!_1Mr[0]){return new F(function(){return _1Md(_);});}else{return E(E(_1Mr[1])[1])==0?E(_1Mr[2])[0]==0?[1,_1HG,[1,_1HI,new T(function(){var _1Ms=E(_1Mb)[1];return _1Ms>0?B(_1Ls(_1LH,_1Ms)):E(_1Lr);})]]:B(_1Md(_)):B(_1Md(_));}}break;case 1:var _1Mt=E(_1LT);if(!_1Mt[0]){var _1Mu=E(_1M3)[1];return _1Mu>0?B(_1HJ(_1Mu,_1g,new T(function(){return B(_8X(_1Hs,_1M2));},1))):B(unAppCStr("0.",new T(function(){var _1Mv= -_1Mu;if(_1Mv>0){var _1Mw=function(_1Mx){return _1Mx>1?[1,_1HG,new T(function(){return B(_1Mw(_1Mx-1|0));})]:E([1,_1HG,new T(function(){return B(_8X(_1Hs,_1M2));})]);},_1My=B(_1Mw(_1Mv));}else{var _1My=B(_8X(_1Hs,_1M2));}var _1Mz=_1My,_1MA=_1Mz;return _1MA;})));}else{var _1MB=_1Mt[1],_1MC=E(_1M3),_1MD=_1MC[1];if(_1MD<0){var _1ME=new T(function(){var _1MF= -_1MD;if(_1MF>0){var _1MG=function(_1MH){return _1MH>1?[1,_LF,new T(function(){return B(_1MG(_1MH-1|0));})]:E([1,_LF,_1M2]);},_1MI=B(_1L3(_1Lq,new T(function(){var _1MJ=E(_1MB);return _1MJ[1]>0?E(_1MJ):E(_LF);},1),B(_1MG(_1MF)))),_1MK=B(_1Hv(_1MI[1],_1MI[2]));}else{var _1ML=B(_1L3(_1Lq,new T(function(){var _1MM=E(_1MB);return _1MM[1]>0?E(_1MM):E(_LF);},1),_1M2)),_1MK=B(_1Hv(_1ML[1],_1ML[2]));}var _1MN=_1MK,_1MO=_1MN;return _1MO;});return [1,new T(function(){return E(E(_1ME)[1]);}),new T(function(){var _1MP=E(E(_1ME)[2]);return _1MP[0]==0?[0]:[1,_1HI,_1MP];})];}else{var _1MQ=B(_1L3(_1Lq,new T(function(){var _1MR=E(_1MB)[1];if(_1MR>0){var _1MS=[0,_1MR+_1MD|0];}else{var _1MS=E(_1MC);}var _1MT=_1MS,_1MU=_1MT;return _1MU;},1),_1M2)),_1MV=_1MQ[2],_1MW=_1MD+E(_1MQ[1])[1]|0;if(_1MW>=0){var _1MX=B(_j9(_1MW,new T(function(){return B(_8X(_1Hs,_1MV));}))),_1MY=_1MX[2],_1MZ=E(_1MX[1]);return _1MZ[0]==0?[1,_1HG,new T(function(){var _1N0=E(_1MY);return _1N0[0]==0?[0]:[1,_1HI,_1N0];})]:B(_5B(_1MZ,new T(function(){var _1N1=E(_1MY);return _1N1[0]==0?[0]:[1,_1HI,_1N1];},1)));}else{return [1,_1HG,new T(function(){var _1N2=B(_8X(_1Hs,_1MV));return _1N2[0]==0?[0]:[1,_1HI,_1N2];})];}}}break;default:var _1N3=E(_1M3),_1N4=_1N3[1];if(_1N4>=0){if(_1N4<=7){_1LX=_1Lp;var _1N5=_1M2;_1LZ=_1N3;_1LY=_1N5;return null;}else{_1LX=_1Lo;var _1N5=_1M2;_1LZ=_1N3;_1LY=_1N5;return null;}}else{_1LX=_1Lo;var _1N5=_1M2;_1LZ=_1N3;_1LY=_1N5;return null;}}})(_1LX,_1LY,_1LZ);if(_1M0!=null){return _1M0;}}},_1N6=function(_1N7){return [1,_Lr,new T(function(){var _1N8=B(_1Jx(E(E(E(E(_1LJ)[1])[2])[1])[1],_1LK,_1LL,_1LM,_1LN,_1LO,_1IY,new T(function(){return B(A(_1LF,[_1LV,_1LU]));})));return B(_1LW(_1LS,_1N8[1],_1N8[2]));})];};if(!B(A(_1Lm,[B(_1Dy(B(_1HE(_1LJ)))),_1LU,new T(function(){return B(A(_Y8,[_1LV,_HF]));})]))){if(!B(A(_1LR,[_1LU]))){var _1N9=B(_1Jx(E(E(E(E(_1LJ)[1])[2])[1])[1],_1LK,_1LL,_1LM,_1LN,_1LO,_1IY,_1LU));return new F(function(){return _1LW(_1LS,_1N9[1],_1N9[2]);});}else{return new F(function(){return _1N6(_);});}}else{return new F(function(){return _1N6(_);});}}else{return !B(A(_1Lm,[B(_1Dy(B(_1HE(_1LJ)))),_1LU,new T(function(){return B(A(_Y8,[_1LV,_HF]));})]))?E(_1Lz):E(_1LA);}}else{return E(_1LB);}},_1Na=function(_1Nb){var _1Nc=u_towlower(_1Nb),_1Nd=_1Nc;return _1Nd>>>0>1114111?B(_rm(_1Nd)):_1Nd;},_1Ne=function(_1Nf){return new F(function(){return err(B(unAppCStr("Printf.printf: ",_1Nf)));});},_1Ng=new T(function(){return B(unCStr("bad argument"));}),_1Nh=new T(function(){return B(_1Ne(_1Ng));}),_1Ni=new T(function(){return B(unCStr("Printf.dfmt: impossible"));}),_1Nj=new T(function(){return B(err(_1Ni));}),_1Nk=[0,45],_1Nl=[1,_1Nk,_1g],_1Nm=new T(function(){return B(err(_1Ni));}),_1Nn=new T(function(){return B(unCStr("Negative exponent"));}),_1No=new T(function(){return B(err(_1Nn));}),_1Np=function(_1Nq,_1Nr){var _1Ns=E(_1Nq);return _1Ns[0]==0?function(_b3){return new F(function(){return _5B(new T(function(){var _1Nt=B(_1FL(E(_1Nr)[1])),_1Nu=jsShow(B(_I1(_1Nt[1],_1Nt[2]))[1]),_1Nv=_1Nu;return fromJSStr(_1Nv);}),_b3);});}:function(_b3){return new F(function(){return _5B(new T(function(){var _1Nw=E(E(_1Ns[1])[1]);if(!_1Nw){var _1Nx=jsRound(E(_1Nr)[1]),_1Ny=_1Nx,_1Nz=jsShow(_1Ny),_1NA=_1Nz,_1NB=fromJSStr(_1NA);}else{var _1NC=B(_1FL(E(_1Nr)[1]));if(_1Nw>=0){var _1ND=B(_1Ha(10,_1Nw)),_1NE=jsRound(B(_I1(_1NC[1],_1NC[2]))[1]*_1ND),_1NF=_1NE,_1NG=jsShow(_1NF/_1ND),_1NH=_1NG,_1NI=fromJSStr(_1NH);}else{var _1NI=E(_1No);}var _1NJ=_1NI,_1NK=_1NJ,_1NL=_1NK,_1NM=_1NL,_1NB=_1NM;}var _1NN=_1NB;return _1NN;}),_b3);});};},_1NO=function(_1NP){return [0,B(_140(E(_1NP)[1]))];},_1NQ=function(_1NR,_1NS,_1NT){var _1NU=E(_1NT);switch(_1NU[0]){case 3:var _1NV=_1NU[1],_1NW=u_iswupper(_1NR),_1NX=_1NW;switch(B(_1Na(_1NR))){case 101:var _1NY=B(_1LI(_1H1,_1zX,_1As,_1Aq,_1Ax,_1Am,_1AD,_1Az,_1AH,_1Lo,new T(function(){var _1NZ=E(_1NS);return _1NZ[1]>=0?[1,_1NZ]:[0];}),_1NV));break;case 102:var _1NY=B(_1LI(_1H1,_1zX,_1As,_1Aq,_1Ax,_1Am,_1AD,_1Az,_1AH,_1Lp,new T(function(){var _1O0=E(_1NS);return _1O0[1]>=0?[1,_1O0]:[0];}),_1NV));break;case 103:var _1O1=E(_1NS),_1NY=_1O1[1]>=0?B(A(_1Np,[[1,_1O1],_1NV,_1g])):B(A(_1Np,[_5A,_1NV,_1g]));break;default:var _1NY=E(_1Nm);}var _1O2=_1NY,_1O3=E(_1NX);if(!_1O3){var _1O4=E(_1O2);if(!_1O4[0]){return [0,_1g,_1g];}else{var _1O5=_1O4[1],_1O6=_1O4[2],_1O7=E(_1O5),_1O8=_1O7[1],_1O9=E(_1O8);return _1O9==45?[0,_1Nl,_1O6]:[0,_1g,_1O4];}}else{var _1Oa=B(_8X(_1NO,_1O2));if(!_1Oa[0]){return [0,_1g,_1g];}else{var _1Ob=_1Oa[1],_1Oc=_1Oa[2],_1Od=E(_1Ob),_1Oe=_1Od[1],_1Of=E(_1Oe);return _1Of==45?[0,_1Nl,_1Oc]:[0,_1g,_1Oa];}}break;case 4:var _1Og=_1NU[1],_1Oh=u_iswupper(_1NR),_1Oi=_1Oh;switch(B(_1Na(_1NR))){case 101:var _1Oj=B(_1LI(_1F2,_1yC,_1A5,_1A2,_1A8,_1zY,_1Ae,_1Aa,_1Ai,_1Lo,new T(function(){var _1Ok=E(_1NS);return _1Ok[1]>=0?[1,_1Ok]:[0];}),_1Og));break;case 102:var _1Oj=B(_1LI(_1F2,_1yC,_1A5,_1A2,_1A8,_1zY,_1Ae,_1Aa,_1Ai,_1Lp,new T(function(){var _1Ol=E(_1NS);return _1Ol[1]>=0?[1,_1Ol]:[0];}),_1Og));break;case 103:var _1Om=E(_1NS)[1];if(_1Om>=0){var _1On=E(_1Om);if(!_1On){var _1Oo=jsRound(E(_1Og)[1]),_1Op=_1Oo,_1Oq=jsShow(_1Op),_1Or=_1Oq,_1Os=fromJSStr(_1Or);}else{var _1Ot=B(_1Ha(10,_1On)),_1Ou=jsRound(E(_1Og)[1]*_1Ot),_1Ov=_1Ou,_1Ow=jsShow(_1Ov/_1Ot),_1Ox=_1Ow,_1Os=fromJSStr(_1Ox);}var _1Oy=_1Os;}else{var _1Oz=jsShow(E(_1Og)[1]),_1OA=_1Oz,_1Oy=fromJSStr(_1OA);}var _1OB=_1Oy,_1OC=_1OB,_1Oj=_1OC;break;default:var _1Oj=E(_1Nj);}var _1OD=_1Oj,_1OE=E(_1Oi);if(!_1OE){var _1OF=E(_1OD);if(!_1OF[0]){return [0,_1g,_1g];}else{var _1OG=_1OF[1],_1OH=_1OF[2],_1OI=E(_1OG),_1OJ=_1OI[1],_1OK=E(_1OJ);return _1OK==45?[0,_1Nl,_1OH]:[0,_1g,_1OF];}}else{var _1OL=B(_8X(_1NO,_1OD));if(!_1OL[0]){return [0,_1g,_1g];}else{var _1OM=_1OL[1],_1ON=_1OL[2],_1OO=E(_1OM),_1OP=_1OO[1],_1OQ=E(_1OP);return _1OQ==45?[0,_1Nl,_1ON]:[0,_1g,_1OL];}}break;default:return E(_1Nh);}},_1OR=[0,0],_1OS=function(_1OT){while(1){var _1OU=E(_1OT);if(!_1OU[0]){_1OT=[1,I_fromInt(_1OU[1])];continue;}else{return new F(function(){return I_toString(_1OU[1]);});}}},_1OV=function(_1OW,_1OX){return new F(function(){return _5B(fromJSStr(B(_1OS(_1OW))),_1OX);});},_1OY=[0,0],_1OZ=function(_1P0,_1P1,_1P2){return _1P0<=6?B(_1OV(_1P1,_1P2)):!B(_HT(_1P1,_1OY))?B(_1OV(_1P1,_1P2)):[1,_5L,new T(function(){return B(_5B(fromJSStr(B(_1OS(_1P1))),[1,_5K,_1P2]));})];},_1P3=function(_1P4){return new F(function(){return _1OZ(0,_1P4,_1g);});},_1P5=[0,48],_1P6=function(_1P7,_1P8){var _1P9=_1P7-B(_I4(_1P8,0))|0;if(_1P9>0){var _1Pa=function(_1Pb){return _1Pb>1?[1,_1P5,new T(function(){return B(_1Pa(_1Pb-1|0));})]:E([1,_1P5,_1P8]);};return new F(function(){return _1Pa(_1P9);});}else{return E(_1P8);}},_1Pc=[0,0],_1Pd=[0,-2147483648],_1Pe=function(_1Pf,_1Pg){while(1){var _1Ph=(function(_1Pi,_1Pj){var _1Pk=E(_1Pj);switch(_1Pk[0]){case 0:_1Pf=_1Pc;_1Pg=[2,_1Pd,new T(function(){return B(_q9(E(_1Pk[1])[1]));})];return null;case 2:var _1Pl=_1Pk[2];return !B(_HT(_1Pl,_1OR))?[0,_1g,new T(function(){return B(_1P6(E(_1Pi)[1],B(_1P3(_1Pl))));})]:[0,_1Nl,new T(function(){return B(_1P6(E(_1Pi)[1],B(_1OZ(0,B(_q3(_1Pl)),_1g))));})];default:return E(_1Nh);}})(_1Pf,_1Pg);if(_1Ph!=null){return _1Ph;}}},_1Pm=[1,_Go,_1g],_1Pn=function(_1Po){return new F(function(){return err(B(unAppCStr("Char.digitToInt: not a digit ",new T(function(){var _1Pp=E(_1Po);return _1Pp==39?E(_Gq):[1,_Go,new T(function(){return B(_G8(_1Pp,_1Pm));})];}))));});},_1Pq=function(_1Pr){var _1Ps=function(_1Pt){var _1Pu=function(_1Pv){if(_1Pr<65){return new F(function(){return _1Pn(_1Pr);});}else{if(_1Pr>70){return new F(function(){return _1Pn(_1Pr);});}else{return (_1Pr-65|0)+10|0;}}};if(_1Pr<97){return new F(function(){return _1Pu(_);});}else{if(_1Pr>102){return new F(function(){return _1Pu(_);});}else{return (_1Pr-97|0)+10|0;}}};if(_1Pr<48){return new F(function(){return _1Ps(_);});}else{if(_1Pr>57){return new F(function(){return _1Ps(_);});}else{return _1Pr-48|0;}}},_1Pw=function(_1Px,_1Py){while(1){var _1Pz=(function(_1PA,_1PB){var _1PC=E(_1PB);if(!_1PC[0]){return [0,_1PA,_1g];}else{var _1PD=E(_1PC[1])[1];if(_1PD<48){return [0,_1PA,_1PC];}else{if(_1PD>57){return [0,_1PA,_1PC];}else{_1Px=new T(function(){return [0,(imul(E(_1PA)[1],10)|0)+B(_1Pq(_1PD))|0];});_1Py=_1PC[2];return null;}}}})(_1Px,_1Py);if(_1Pz!=null){return _1Pz;}}},_1PE=new T(function(){return B(unCStr("argument list ended prematurely"));}),_1PF=new T(function(){return B(_1Ne(_1PE));}),_1PG=[0,-1],_1PH=function(_1PI){return [0,E(_1PI)[1]];},_1PJ=function(_1PK){var _1PL=E(_1PK);switch(_1PL[0]){case 0:return new F(function(){return _1PH(_1PL[1]);});break;case 2:return new F(function(){return _1CC(_1PL[2]);});break;default:return E(_1Nh);}},_1PM=function(_1PN,_1PO,_1PP,_1PQ,_1PR){while(1){var _1PS=(function(_1PT,_1PU,_1PV,_1PW,_1PX){var _1PY=E(_1PW);if(!_1PY[0]){return [0,_1Pc,_1PG,_1PT,_1PU,_1PV,_1g,_1PX];}else{var _1PZ=_1PY[2],_1Q0=E(E(_1PY[1])[1]);switch(_1Q0){case 42:var _1Q1=new T(function(){var _1Q2=E(_1PX);return _1Q2[0]==0?E(_1PF):[0,_1Q2[2],new T(function(){return B(_1PJ(_1Q2[1]));})];}),_1Q3=new T(function(){var _1Q4=E(_1PZ);if(!_1Q4[0]){var _1Q5=[0,_1PG,_1g,new T(function(){return E(E(_1Q1)[1]);})];}else{if(E(E(_1Q4[1])[1])==46){var _1Q6=E(_1Q4[2]);if(!_1Q6[0]){var _1Q7=B(_1Pw(_1Pc,_1g)),_1Q8=[0,_1Q7[1],_1Q7[2],new T(function(){return E(E(_1Q1)[1]);})];}else{if(E(E(_1Q6[1])[1])==42){var _1Q9=new T(function(){var _1Qa=E(E(_1Q1)[1]);return _1Qa[0]==0?E(_1PF):[0,_1Qa[2],new T(function(){return B(_1PJ(_1Qa[1]));})];}),_1Qb=[0,new T(function(){return E(E(_1Q9)[2]);}),_1Q6[2],new T(function(){return E(E(_1Q9)[1]);})];}else{var _1Qc=B(_1Pw(_1Pc,_1Q6)),_1Qb=[0,_1Qc[1],_1Qc[2],new T(function(){return E(E(_1Q1)[1]);})];}var _1Qd=_1Qb,_1Q8=_1Qd;}var _1Qe=_1Q8;}else{var _1Qe=[0,_1PG,_1Q4,new T(function(){return E(E(_1Q1)[1]);})];}var _1Qf=_1Qe,_1Q5=_1Qf;}return _1Q5;});return [0,new T(function(){return E(E(_1Q1)[2]);}),new T(function(){return E(E(_1Q3)[1]);}),_1PT,_1PU,_1PV,new T(function(){return E(E(_1Q3)[2]);}),new T(function(){return E(E(_1Q3)[3]);})];case 43:var _1Qg=_1PT,_1Qh=_1PU;_1PP=_fI;_1PQ=_1PZ;var _1Qi=_1PX;_1PN=_1Qg;_1PO=_1Qh;_1PR=_1Qi;return null;case 45:_1PN=_fI;var _1Qh=_1PU,_1Qj=_1PV;_1PQ=_1PZ;var _1Qi=_1PX;_1PO=_1Qh;_1PP=_1Qj;_1PR=_1Qi;return null;case 46:var _1Qk=new T(function(){var _1Ql=E(_1PZ);if(!_1Ql[0]){var _1Qm=B(_1Pw(_1Pc,_1g)),_1Qn=[0,_1Qm[1],_1Qm[2],_1PX];}else{if(E(E(_1Ql[1])[1])==42){var _1Qo=new T(function(){var _1Qp=E(_1PX);return _1Qp[0]==0?E(_1PF):[0,_1Qp[2],new T(function(){return B(_1PJ(_1Qp[1]));})];}),_1Qq=[0,new T(function(){return E(E(_1Qo)[2]);}),_1Ql[2],new T(function(){return E(E(_1Qo)[1]);})];}else{var _1Qr=B(_1Pw(_1Pc,_1Ql)),_1Qq=[0,_1Qr[1],_1Qr[2],_1PX];}var _1Qs=_1Qq,_1Qn=_1Qs;}return _1Qn;});return [0,_1Pc,new T(function(){return E(E(_1Qk)[1]);}),_1PT,_1PU,_1PV,new T(function(){return E(E(_1Qk)[2]);}),new T(function(){return E(E(_1Qk)[3]);})];case 48:var _1Qg=_1PT;_1PO=_fI;var _1Qj=_1PV;_1PQ=_1PZ;var _1Qi=_1PX;_1PN=_1Qg;_1PP=_1Qj;_1PR=_1Qi;return null;default:if(_1Q0<48){return [0,_1Pc,_1PG,_1PT,_1PU,_1PV,_1PY,_1PX];}else{if(_1Q0>57){return [0,_1Pc,_1PG,_1PT,_1PU,_1PV,_1PY,_1PX];}else{var _1Qt=new T(function(){var _1Qu=B(_1Pw(_1Pc,_1PY));return [0,_1Qu[1],_1Qu[2]];}),_1Qv=new T(function(){var _1Qw=E(E(_1Qt)[2]);if(!_1Qw[0]){var _1Qx=[0,_1PG,_1g,_1PX];}else{if(E(E(_1Qw[1])[1])==46){var _1Qy=E(_1Qw[2]);if(!_1Qy[0]){var _1Qz=B(_1Pw(_1Pc,_1g)),_1QA=[0,_1Qz[1],_1Qz[2],_1PX];}else{if(E(E(_1Qy[1])[1])==42){var _1QB=new T(function(){var _1QC=E(_1PX);return _1QC[0]==0?E(_1PF):[0,_1QC[2],new T(function(){return B(_1PJ(_1QC[1]));})];}),_1QD=[0,new T(function(){return E(E(_1QB)[2]);}),_1Qy[2],new T(function(){return E(E(_1QB)[1]);})];}else{var _1QE=B(_1Pw(_1Pc,_1Qy)),_1QD=[0,_1QE[1],_1QE[2],_1PX];}var _1QF=_1QD,_1QA=_1QF;}var _1QG=_1QA;}else{var _1QG=[0,_1PG,_1Qw,_1PX];}var _1QH=_1QG,_1Qx=_1QH;}var _1QI=_1Qx;return _1QI;});return [0,new T(function(){return E(E(_1Qt)[1]);}),new T(function(){return E(E(_1Qv)[1]);}),_1PT,_1PU,_1PV,new T(function(){return E(E(_1Qv)[2]);}),new T(function(){return E(E(_1Qv)[3]);})];}}}}})(_1PN,_1PO,_1PP,_1PQ,_1PR);if(_1PS!=null){return _1PS;}}},_1QJ=new T(function(){return B(unCStr("formatting string ended prematurely"));}),_1QK=new T(function(){return B(_1Ne(_1QJ));}),_1QL=function(_1QM,_1QN){if(!B(_HT(_1QN,_1QM))){if(!B(_HL(_1QM,_1OR))){var _1QO=B(_10q(_1QN,_1QM));return new F(function(){return _5B(B(_1QL(_1QM,_1QO[1])),[1,new T(function(){return [0,B(_1Ho(B(_ro(_1QO[2]))))];}),_1g]);});}else{return E(_IO);}}else{return [1,new T(function(){return [0,B(_1Ho(B(_ro(_1QN))))];}),_1g];}},_1QP=[0,2],_1QQ=function(_1QR,_1QS,_1QT){var _1QU=E(_1QT);switch(_1QU[0]){case 0:return new F(function(){return _1QL(_1QR,B(_q9(E(_1QU[1])[1])));});break;case 2:var _1QV=_1QU[2],_1QW=E(_1QS)[1];if(!B(_HT(_1QV,_1OR))){return new F(function(){return _1P6(_1QW,B(_1QL(_1QR,_1QV)));});}else{return new F(function(){return _1P6(_1QW,B(_1QL(_1QR,B(_pT(B(_q3(B(_qb(_1QP,_1QU[1])))),_1QV)))));});}break;default:return E(_1Nh);}},_1QX=[0,37],_1QY=[0,16],_1QZ=[0,10],_1R0=[0,8],_1R1=[0,43],_1R2=[1,_1R1,_1g],_1R3=[0,32],_1R4=function(_1R5){return new F(function(){return _1Ne(new T(function(){return B(unAppCStr("bad formatting char ",[1,_1R5,_1g]));}));});},_1R6=function(_1R7,_1R8){var _1R9=E(_1R7);if(!_1R9[0]){return E(_1R8)[0]==0?[0]:E(_1QK);}else{var _1Ra=_1R9[2],_1Rb=E(_1R9[1]);if(E(_1Rb[1])==37){var _1Rc=function(_1Rd){var _1Re=E(_1R8);if(!_1Re[0]){return E(_1PF);}else{var _1Rf=B(_1PM(_61,_61,_61,_1Ra,_1Re)),_1Rg=_1Rf[2],_1Rh=_1Rf[4],_1Ri=E(_1Rf[6]);if(!_1Ri[0]){return E(_1QK);}else{var _1Rj=_1Ri[2],_1Rk=E(_1Rf[7]);if(!_1Rk[0]){return E(_1PF);}else{var _1Rl=_1Rk[1],_1Rm=_1Rk[2],_1Rn=E(_1Ri[1]),_1Ro=function(_1Rp,_1Rq){var _1Rr=new T(function(){var _1Rs=B(_I4(_1Rq,0)),_1Rt=B(_I4(_1Rp,0)),_1Ru=E(_1Rf[1])[1];if((_1Rs+_1Rt|0)>=_1Ru){var _1Rv=[0];}else{var _1Rw=_1Ru-(_1Rs+_1Rt|0)|0;if(_1Rw>0){if(_1Rw<0){var _1Rx=[0];}else{var _1Ry=new T(function(){return [1,new T(function(){return !E(_1Rh)?E(_1R3):E(_1P5);}),_1Ry];}),_1Rx=B(_1s8(_1Rw,_1Ry));}var _1Rz=_1Rx,_1RA=_1Rz;}else{var _1RA=[0];}var _1RB=_1RA,_1RC=_1RB,_1RD=_1RC,_1Rv=_1RD;}var _1RE=_1Rv,_1RF=_1RE,_1RG=_1RF,_1RH=_1RG,_1RI=_1RH;return _1RI;},1);return !E(_1Rf[3])?!E(_1Rh)?B(_5B(_1Rr,new T(function(){return B(_5B(_1Rp,_1Rq));},1))):B(_5B(_1Rp,new T(function(){return B(_5B(_1Rr,_1Rq));},1))):B(_5B(_1Rp,new T(function(){return B(_5B(_1Rq,_1Rr));},1)));},_1RJ=function(_1RK,_1RL){var _1RM=E(_1RK);return _1RM[0]==0?!E(_1Rf[5])?B(_1Ro(_1g,_1RL)):B(_1Ro(_1R2,_1RL)):B(_1Ro(_1RM,_1RL));};switch(E(_1Rn[1])){case 69:var _1RN=B(_1NQ(69,_1Rg,_1Rl));return new F(function(){return _5B(B(_1RJ(_1RN[1],_1RN[2])),new T(function(){return B(_1R6(_1Rj,_1Rm));},1));});break;case 71:var _1RO=B(_1NQ(71,_1Rg,_1Rl));return new F(function(){return _5B(B(_1RJ(_1RO[1],_1RO[2])),new T(function(){return B(_1R6(_1Rj,_1Rm));},1));});break;case 88:return new F(function(){return _5B(B(_1Ro(_1g,new T(function(){return B(_8X(_1NO,B(_1QQ(_1QY,_1Rg,_1Rl))));}))),new T(function(){return B(_1R6(_1Rj,_1Rm));},1));});break;case 99:return new F(function(){return _5B(B(_1Ro(_1g,[1,new T(function(){var _1RP=E(_1Rl);switch(_1RP[0]){case 0:var _1RQ=E(_1RP[1])[1];if(_1RQ>>>0>1114111){var _1RR=B(_rm(_1RQ));}else{var _1RR=[0,_1RQ];}var _1RS=_1RR,_1RT=_1RS,_1RU=_1RT,_1RV=_1RU,_1RW=_1RV;break;case 2:var _1RX=B(_ro(_1RP[2]));if(_1RX>>>0>1114111){var _1RY=B(_rm(_1RX));}else{var _1RY=[0,_1RX];}var _1RZ=_1RY,_1S0=_1RZ,_1S1=_1S0,_1RW=_1S1;break;default:var _1RW=E(_1Nh);}return _1RW;}),_1g])),new T(function(){return B(_1R6(_1Rj,_1Rm));},1));});break;case 100:var _1S2=B(_1Pe(_1Rg,_1Rl));return new F(function(){return _5B(B(_1RJ(_1S2[1],_1S2[2])),new T(function(){return B(_1R6(_1Rj,_1Rm));},1));});break;case 101:var _1S3=B(_1NQ(101,_1Rg,_1Rl));return new F(function(){return _5B(B(_1RJ(_1S3[1],_1S3[2])),new T(function(){return B(_1R6(_1Rj,_1Rm));},1));});break;case 102:var _1S4=B(_1NQ(102,_1Rg,_1Rl));return new F(function(){return _5B(B(_1RJ(_1S4[1],_1S4[2])),new T(function(){return B(_1R6(_1Rj,_1Rm));},1));});break;case 103:var _1S5=B(_1NQ(103,_1Rg,_1Rl));return new F(function(){return _5B(B(_1RJ(_1S5[1],_1S5[2])),new T(function(){return B(_1R6(_1Rj,_1Rm));},1));});break;case 105:var _1S6=B(_1Pe(_1Rg,_1Rl));return new F(function(){return _5B(B(_1RJ(_1S6[1],_1S6[2])),new T(function(){return B(_1R6(_1Rj,_1Rm));},1));});break;case 111:return new F(function(){return _5B(B(_1Ro(_1g,new T(function(){return B(_1QQ(_1R0,_1Rg,_1Rl));}))),new T(function(){return B(_1R6(_1Rj,_1Rm));},1));});break;case 115:return new F(function(){return _5B(B(_1Ro(_1g,new T(function(){var _1S7=E(_1Rl);if(_1S7[0]==1){var _1S8=_1S7[1],_1S9=E(_1Rg)[1];if(_1S9<0){var _1Sa=E(_1S8);}else{var _1Sa=_1S9>0?B(_1s8(_1S9,_1S8)):[0];}var _1Sb=_1Sa,_1Sc=_1Sb,_1Sd=_1Sc;}else{var _1Sd=E(_1Nh);}return _1Sd;}))),new T(function(){return B(_1R6(_1Rj,_1Rm));},1));});break;case 117:return new F(function(){return _5B(B(_1Ro(_1g,new T(function(){return B(_1QQ(_1QZ,_1Rg,_1Rl));}))),new T(function(){return B(_1R6(_1Rj,_1Rm));},1));});break;case 120:return new F(function(){return _5B(B(_1Ro(_1g,new T(function(){return B(_1QQ(_1QY,_1Rg,_1Rl));}))),new T(function(){return B(_1R6(_1Rj,_1Rm));},1));});break;default:return new F(function(){return _1R4(_1Rn);});}}}}},_1Se=E(_1Ra);if(!_1Se[0]){return new F(function(){return _1Rc(_);});}else{if(E(E(_1Se[1])[1])==37){return [1,_1QX,new T(function(){return B(_1R6(_1Se[2],_1R8));})];}else{return new F(function(){return _1Rc(_);});}}}else{return [1,_1Rb,new T(function(){return B(_1R6(_1Ra,_1R8));})];}}},_1Sf=[0,48],_1Sg=[1,_1Sf,_1g],_1Sh=function(_1Si){return _1Si>1?[1,_1Sf,new T(function(){return B(_1Sh(_1Si-1|0));})]:E(_1Sg);},_1Sj=function(_1Sk){return _1Sk>1?[1,_1Sf,new T(function(){return B(_1Sj(_1Sk-1|0));})]:E(_1Sg);},_1Sl=function(_1Sm,_1Sn){var _1So=B(_8X(_1xP,B(_1R6(B(unAppCStr("%.",new T(function(){return B(_5B(B(_5M(0,E(_1Sm)[1],_1g)),_1xS));}))),new T(function(){return B(_1xT([1,[4,_1Sn],_1g],_1g));},1)))));return !B(_h5(_1So,B(unAppCStr("-0.",new T(function(){var _1Sp=E(_1Sm)[1];return _1Sp>0?B(_1Sj(_1Sp)):[0];})))))?E(_1So):B(unAppCStr("0.",new T(function(){var _1Sq=E(_1Sm)[1];return _1Sq>0?B(_1Sh(_1Sq)):[0];})));},_1Sr=function(_1Ss,_1St){var _1Su=E(_1Ss);if(!_1Su[0]){return [0];}else{var _1Sv=E(_1St);return _1Sv[0]==0?[0]:[1,[0,_1Su[1],_1Sv[1]],new T(function(){return B(_1Sr(_1Su[2],_1Sv[2]));})];}},_1Sw=function(_1Sx,_1Sy,_1Sz,_1SA,_1SB,_1SC,_1SD,_1SE){var _1SF=new T(function(){var _1SG=E(_1Sz);if(!_1SG[0]){var _1SH=E(_1jR);}else{var _1SH=E(E(_1SG[1])[1]);}return _1SH;}),_1SI=new T(function(){var _1SJ=E(_1SF);if(!_1SJ[0]){var _1SK=E(_1xO);}else{var _1SL=E(_1SJ[1]),_1SM=_1SL[1],_1SN=_1SL[2],_1SO=E(_1SJ[2]);if(!_1SO[0]){var _1SP=E(_1SM);}else{var _1SQ=_1SO[2],_1SR=E(_1SM)[1],_1SS=E(_1SO[1]),_1ST=E(_1SS[1])[1];if(_1SR>=_1ST){if(_1SR!=_1ST){var _1SU=E(B(_1vo(_1ST,_1SS[2],_1SQ))[1]);}else{var _1SU=E(B(_1vo(_1SR,_1SN,_1SQ))[1]);}var _1SV=_1SU,_1SW=_1SV;}else{var _1SW=E(B(_1vo(_1SR,_1SN,_1SQ))[1]);}var _1SX=_1SW,_1SY=_1SX,_1SZ=_1SY,_1T0=_1SZ,_1SP=_1T0;}var _1T1=_1SP,_1SK=_1T1;}return _1SK;}),_1T2=new T(function(){var _1T3=E(_1SF);if(!_1T3[0]){var _1T4=E(_8c);}else{var _1T5=E(_1T3[1]),_1T6=_1T5[1],_1T7=E(_1T3[2]);if(!_1T7[0]){var _1T8=E(_1T6)[1]-E(_1SI)[1],_1T9=_1T8>=1.0e-3?[0,_1T8]:E(_1xK);}else{var _1Ta=_1T7[2],_1Tb=E(_1T6)[1],_1Tc=E(_1T7[1]),_1Td=_1Tc[2],_1Te=E(_1Tc[1])[1];if(_1Tb>=_1Te){if(_1Tb!=_1Te){var _1Tf=E(B(_1vw(_1Tb,_1T5[2],_1Ta))[1])[1]-E(_1SI)[1],_1Tg=_1Tf>=1.0e-3?[0,_1Tf]:E(_1xK);}else{var _1Th=E(B(_1vw(_1Te,_1Td,_1Ta))[1])[1]-E(_1SI)[1],_1Tg=_1Th>=1.0e-3?[0,_1Th]:E(_1xK);}var _1Ti=_1Tg,_1Tj=_1Ti;}else{var _1Tk=E(B(_1vw(_1Te,_1Td,_1Ta))[1])[1]-E(_1SI)[1],_1Tj=_1Tk>=1.0e-3?[0,_1Tk]:E(_1xK);}var _1Tl=_1Tj,_1Tm=_1Tl,_1Tn=_1Tm,_1To=_1Tn,_1T9=_1To;}var _1Tp=_1T9,_1T4=_1Tp;}return _1T4;}),_1Tq=new T(function(){var _1Tr=E(_1SF);if(!_1Tr[0]){var _1Ts=E(_1xO);}else{var _1Tt=E(_1Tr[1]),_1Tu=_1Tt[1],_1Tv=_1Tt[2],_1Tw=E(_1Tr[2]);if(!_1Tw[0]){var _1Tx=E(_1Tv);}else{var _1Ty=_1Tw[2],_1Tz=E(_1Tv)[1],_1TA=E(_1Tw[1]),_1TB=E(_1TA[2])[1];if(_1Tz>=_1TB){if(_1Tz!=_1TB){var _1TC=E(B(_1v7(_1TA[1],_1TB,_1Ty))[2]);}else{var _1TC=E(B(_1v7(_1Tu,_1Tz,_1Ty))[2]);}var _1TD=_1TC,_1TE=_1TD;}else{var _1TE=E(B(_1v7(_1Tu,_1Tz,_1Ty))[2]);}var _1TF=_1TE,_1TG=_1TF,_1TH=_1TG,_1TI=_1TH,_1Tx=_1TI;}var _1TJ=_1Tx,_1Ts=_1TJ;}return _1Ts;}),_1TK=new T(function(){var _1TL=E(_1SF);if(!_1TL[0]){var _1TM=E(_8c);}else{var _1TN=E(_1TL[1]),_1TO=_1TN[2],_1TP=E(_1TL[2]);if(!_1TP[0]){var _1TQ=E(_1TO)[1]-E(_1Tq)[1],_1TR=_1TQ>=1.0e-3?[0,_1TQ]:E(_1xK);}else{var _1TS=_1TP[2],_1TT=E(_1TO)[1],_1TU=E(_1TP[1]),_1TV=_1TU[1],_1TW=E(_1TU[2])[1];if(_1TT>=_1TW){if(_1TT!=_1TW){var _1TX=E(B(_1vf(_1TN[1],_1TT,_1TS))[2])[1]-E(_1Tq)[1],_1TY=_1TX>=1.0e-3?[0,_1TX]:E(_1xK);}else{var _1TZ=E(B(_1vf(_1TV,_1TW,_1TS))[2])[1]-E(_1Tq)[1],_1TY=_1TZ>=1.0e-3?[0,_1TZ]:E(_1xK);}var _1U0=_1TY,_1U1=_1U0;}else{var _1U2=E(B(_1vf(_1TV,_1TW,_1TS))[2])[1]-E(_1Tq)[1],_1U1=_1U2>=1.0e-3?[0,_1U2]:E(_1xK);}var _1U3=_1U1,_1U4=_1U3,_1U5=_1U4,_1U6=_1U5,_1TR=_1U6;}var _1U7=_1TR,_1TM=_1U7;}return _1TM;}),_1U8=function(_1U9){var _1Ua=E(_1U9);return [0,new T(function(){return [0,E(_1SA)[1]*(0.1+0.8*(E(_1Ua[1])[1]-E(_1SI)[1])/E(_1T2)[1])];}),new T(function(){return [0,E(_1SB)[1]*(1-(0.1+0.8*(E(_1Ua[2])[1]-E(_1Tq)[1])/E(_1TK)[1]))];})];},_1Ub=new T(function(){return B(_8X(_1U8,_1SF));}),_1Uc=new T(function(){var _1Ud=E(_1SA)[1];return [0,(_1Ud+_1Ud)/900];}),_1Ue=new T(function(){return B(_1x3([1,[0,new T(function(){return [0, -(2.5e-2*E(_1SA)[1])];}),new T(function(){return [0,1.0e-2*E(_1SB)[1]];})],[1,_1xI,[1,[0,new T(function(){return [0, -(2.5e-2*E(_1SA)[1])];}),new T(function(){return [0, -(1.0e-2*E(_1SB)[1])];})],_1g]]]));});return function(_1Uf,_){var _1Ug=E(_1Uf),_1Uh=_1Ug[1],_1Ui=jsBeginPath(_1Uh),_1Uj=B(A(new T(function(){return B(_1x3([1,[0,_1xH,_1SB],[1,[0,_1SA,_1SB],_1g]]));}),[[0,_1Uh],_])),_1Uk=_1Uj,_1Ul=jsStroke(_1Uh),_1Um=jsPushState(_1Uh),_1Un=jsTranslate(_1Uh,E(_1SA)[1],E(_1SB)[1]),_1Uo=jsBeginPath(_1Uh),_1Up=B(A(_1Ue,[[0,_1Uh],_])),_1Uq=_1Up,_1Ur=jsStroke(_1Uh),_1Us=jsPopState(_1Uh),_1Ut=jsBeginPath(_1Uh),_1Uu=B(A(new T(function(){return B(_1x3([1,[0,_1xH,_1SB],_1xJ]));}),[[0,_1Uh],_])),_1Uv=_1Uu,_1Uw=jsStroke(_1Uh),_1Ux=jsPushState(_1Uh),_1Uy=jsTranslate(_1Uh,0,0),_1Uz=jsPushState(_1Uh),_1UA=jsRotate(_1Uh,-1.5707963267948966),_1UB=jsBeginPath(_1Uh),_1UC=B(A(_1Ue,[[0,_1Uh],_])),_1UD=_1UC,_1UE=jsStroke(_1Uh),_1UF=jsPopState(_1Uh),_1UG=jsPopState(_1Uh),_1UH=jsPushState(_1Uh),_1UI=jsTranslate(_1Uh,E(new T(function(){return [0,0.8*E(_1SA)[1]];}))[1],E(new T(function(){return [0,0.95*E(_1SB)[1]];}))[1]),_1UJ=E(_1Uc)[1],_1UK=jsPushState(_1Uh),_1UL=jsScale(_1Uh,_1UJ,_1UJ),_1UM=jsDrawText(_1Uh,E(new T(function(){return [0,toJSStr(E(_1Sx))];}))[1],0,0),_1UN=jsPopState(_1Uh),_1UO=jsPopState(_1Uh),_1UP=jsPushState(_1Uh),_1UQ=jsTranslate(_1Uh,E(new T(function(){return [0,5.0e-2*E(_1SA)[1]];}))[1],0),_1UR=jsPushState(_1Uh),_1US=jsScale(_1Uh,_1UJ,_1UJ),_1UT=jsDrawText(_1Uh,E(new T(function(){return [0,toJSStr(E(_1Sy))];}))[1],0,0),_1UU=jsPopState(_1Uh),_1UV=jsPopState(_1Uh),_1UW=B(A(new T(function(){var _1UX=new T(function(){return B(_I4(_1Ub,0))>2?[1,new T(function(){var _1UY=E(_1Ub);return _1UY[0]==0?E(_1jR):E(_1UY[1]);}),new T(function(){var _1UZ=E(_1Ub);if(!_1UZ[0]){var _1V0=E(_1qp);}else{var _1V1=B(_1qk(_1UZ[1],_1UZ[2])),_1V0=_1V1[0]==0?E(_al):E(_1V1[2]);}var _1V2=_1V0,_1V3=B(_I4(_1V2,0)),_1V4=function(_1V5){return new F(function(){return _5B(B(_1w1(_1V5,_1V2)),[1,new T(function(){var _1V6=E(_1Ub);return _1V6[0]==0?E(_1xF):B(_1xA(_1V6[1],_1V6[2]));}),_1g]);});},_1V7=10<=_1V3,_1V8=_1V7,_1V9=!_1V8?B(_1V4(_1V3)):B(_1V4(10)),_1Va=_1V9,_1Vb=_1Va,_1Vc=_1Vb;return _1Vc;})]:E(_1Ub);});return B(_1wI(_1xL,function(_1Vd,_){var _1Ve=B(A(new T(function(){var _1Vf=function(_1Vg){var _1Vh=E(_1Vg);if(!_1Vh[0]){return E(_1w7);}else{var _1Vi=new T(function(){return E(E(_1Vh[1])[1]);});return function(_1Vj,_){var _1Vk=E(_1Vj),_1Vl=_1Vk[1],_1Vm=jsBeginPath(_1Vl),_1Vn=B(A(new T(function(){return B(_1x3([1,[0,_1Vi,_1xH],[1,[0,_1Vi,_1SB],_1g]]));}),[[0,_1Vl],_])),_1Vo=_1Vn,_1Vp=jsStroke(_1Vl),_1Vq=jsPushState(_1Vl),_1Vr=jsTranslate(_1Vl,E(new T(function(){var _1Vs=E(_1Vi)[1],_1Vt=B(_I4(B(_1Sl(_1xM,new T(function(){return [0,E(_1SI)[1]+(_1Vs/E(_1SA)[1]-0.1)*E(_1T2)[1]/0.8];}))),0))-1|0;if(4>_1Vt){var _1Vu=[0,_1Vs-_1Vt*5.0e-3*E(_1SA)[1]];}else{var _1Vu=[0,_1Vs-2.0e-2*E(_1SA)[1]];}var _1Vv=_1Vu,_1Vw=_1Vv,_1Vx=_1Vw,_1Vy=_1Vx,_1Vz=_1Vy,_1VA=_1Vz;return _1VA;}))[1],E(new T(function(){return [0,1.05*E(_1SB)[1]];}))[1]),_1VB=E(_1Uc)[1],_1VC=jsPushState(_1Vl),_1VD=jsScale(_1Vl,_1VB,_1VB),_1VE=jsDrawText(_1Vl,E(new T(function(){return [0,toJSStr(B(_1Sl(_1SC,new T(function(){return [0,E(_1SI)[1]+(E(_1Vi)[1]/E(_1SA)[1]-0.1)*E(_1T2)[1]/0.8];}))))];}))[1],0,0),_1VF=jsPopState(_1Vl),_1VG=jsPopState(_1Vl);return new F(function(){return A(new T(function(){return B(_1Vf(_1Vh[2]));}),[_1Vk,_]);});};}};return B(_1Vf(_1UX));}),[_1Vd,_])),_1VH=_1Ve;return new F(function(){return A(new T(function(){var _1VI=function(_1VJ){var _1VK=E(_1VJ);if(!_1VK[0]){return E(_1w7);}else{var _1VL=new T(function(){return E(E(_1VK[1])[2]);});return function(_1VM,_){var _1VN=E(_1VM),_1VO=_1VN[1],_1VP=jsBeginPath(_1VO),_1VQ=B(A(new T(function(){return B(_1x3([1,[0,_1xH,_1VL],[1,[0,_1SA,_1VL],_1g]]));}),[[0,_1VO],_])),_1VR=_1VQ,_1VS=jsStroke(_1VO),_1VT=jsPushState(_1VO),_1VU=jsTranslate(_1VO,E(new T(function(){var _1VV=B(_I4(B(_1Sl(_1xG,new T(function(){return [0,E(_1Tq)[1]+(1-E(_1VL)[1]/E(_1SB)[1]-0.1)*E(_1TK)[1]/0.8];}))),0))-1|0;if(4>_1VV){var _1VW=[0, -(_1VV*2.2e-2*E(_1SA)[1])];}else{var _1VW=[0, -(8.8e-2*E(_1SA)[1])];}var _1VX=_1VW,_1VY=_1VX,_1VZ=_1VY,_1W0=_1VZ,_1W1=_1W0;return _1W1;}))[1],E(new T(function(){return [0,E(_1VL)[1]+1.5e-2*E(_1SB)[1]];}))[1]),_1W2=E(_1Uc)[1],_1W3=jsPushState(_1VO),_1W4=jsScale(_1VO,_1W2,_1W2),_1W5=jsDrawText(_1VO,E(new T(function(){return [0,toJSStr(B(_1Sl(_1SD,new T(function(){return [0,E(_1Tq)[1]+(1-E(_1VL)[1]/E(_1SB)[1]-0.1)*E(_1TK)[1]/0.8];}))))];}))[1],0,0),_1W6=jsPopState(_1VO),_1W7=jsPopState(_1VO);return new F(function(){return A(new T(function(){return B(_1VI(_1VK[2]));}),[_1VN,_]);});};}};return B(_1VI(_1UX));}),[_1Vd,_]);});}));}),[_1Ug,_])),_1W8=_1UW,_1W9=B(A(new T(function(){return B(_1wI(new T(function(){var _1Wa=E(_1Sz);if(!_1Wa[0]){var _1Wb=E(_1jR);}else{var _1Wb=E(E(_1Wa[1])[2]);}return _1Wb;},1),new T(function(){var _1Wc=E(_1Ub);if(!_1Wc[0]){var _1Wd=E(_1w7);}else{var _1We=_1Wc[1];if(B(_I4(_1Wc,0))==1){var _1Wf=function(_1Wg,_){var _1Wh=E(_1Wg)[1],_1Wi=jsBeginPath(_1Wh),_1Wj=B(A(new T(function(){return B(_1x3([1,_1We,[1,_1We,_1g]]));}),[[0,_1Wh],_])),_1Wk=_1Wj,_1Wl=jsStroke(_1Wh);return _c;};}else{var _1Wf=B(_1xe(B(_1Sr(_1Wc,_1Wc[2]))));}var _1Wd=_1Wf;}return _1Wd;})));}),[_1Ug,_])),_1Wm=_1W9,_1Wn=B(A(new T(function(){var _1Wo=E(_1Sz);if(!_1Wo[0]){var _1Wp=E(_al);}else{var _1Wq=function(_1Wr){var _1Ws=E(_1Wr);if(!_1Ws[0]){return E(_1w9);}else{var _1Wt=_1Ws[1];return function(_1Wu,_){var _1Wv=B(A(new T(function(){return B(_1wI(new T(function(){return E(E(_1Wt)[2]);},1),new T(function(){var _1Ww=B(_8X(_1U8,E(_1Wt)[1]));if(!_1Ww[0]){var _1Wx=E(_1w7);}else{var _1Wy=_1Ww[1];if(B(_I4(_1Ww,0))==1){var _1Wz=function(_1WA,_){var _1WB=E(_1WA)[1],_1WC=jsBeginPath(_1WB),_1WD=B(A(new T(function(){return B(_1x3([1,_1Wy,[1,_1Wy,_1g]]));}),[[0,_1WB],_])),_1WE=_1WD,_1WF=jsStroke(_1WB);return _c;};}else{var _1Wz=B(_1xp(B(_1Sr(_1Ww,_1Ww[2]))));}var _1Wx=_1Wz;}var _1WG=_1Wx;return _1WG;})));}),[_1Wu,_])),_1WH=_1Wv,_1WI=B(A(new T(function(){return B(_1Wq(_1Ws[2]));}),[_1Wu,_])),_1WJ=_1WI;return [1,_1WH,_1WJ];};}},_1Wp=B(_1Wq(_1Wo[2]));}return _1Wp;}),[_1Ug,_])),_1WK=_1Wn;return new F(function(){return A(new T(function(){var _1WL=function(_1WM){var _1WN=E(_1WM);return _1WN[0]==0?E(_1w7):function(_1WO,_){var _1WP=B(A(new T(function(){var _1WQ=E(_1WN[1]);return B(_1wI(_1WQ[3],function(_1WR,_){return new F(function(){return _1vM(function(_1WS,_){return new F(function(){return _1vF(E(new T(function(){return [0,E(_1SA)[1]*(0.1+0.8*(E(_1WQ[1])[1]-E(_1SI)[1])/E(_1T2)[1])];}))[1],E(new T(function(){return [0,E(_1SB)[1]*(1-(0.1+0.8*(E(_1WQ[2])[1]-E(_1Tq)[1])/E(_1TK)[1]))];}))[1],E(new T(function(){return [0,5.0e-3*E(_1SA)[1]];}))[1],E(_1WS)[1],_);});},E(_1WR)[1],_);});}));}),[_1WO,_])),_1WT=_1WP;return new F(function(){return A(new T(function(){return B(_1WL(_1WN[2]));}),[_1WO,_]);});};};return B(_1WL(_1SE));}),[_1Ug,_]);});};},_1WU=new T(function(){return B(unCStr("border: 1px solid black;"));}),_1WV=new T(function(){return B(unCStr("canvas"));}),_1WW=function(_1WX,_1WY,_1WZ,_){var _1X0=jsCreateElem(toJSStr(E(_1WV))),_1X1=_1X0,_1X2=jsAppendChild(_1X1,E(_1WZ)[1]),_1X3=[0,_1X1],_1X4=B(A(_1WX,[_1WY,_1X3,_])),_1X5=_1X4;return _1X3;},_1X6=function(_1X7,_){return [0,[0,_K,[1,_1X7]],_1X7];},_1X8=function(_1X9){return function(_1Xa,_){return [0,[0,_K,[1,[1,_5S,new T(function(){var _1Xb=E(_1X9);return B(_5B(B(_5M(0,E(_1Xb[2])[1],_1g)),_1Xb[1]));})]]],_1Xa];};},_1Xc=new T(function(){return B(unCStr("canvas"));}),_1Xd=function(_1Xe,_){var _1Xf=B(_76(_1X6,_1X8,_1Xe,_)),_1Xg=_1Xf;return [0,new T(function(){var _1Xh=E(E(_1Xg)[1]);return [0,_1Xh[1],new T(function(){var _1Xi=E(_1Xh[2]);return _1Xi[0]==0?[0]:[1,new T(function(){return B(_5B(_1Xc,_1Xi[1]));})];})];}),new T(function(){return E(E(_1Xg)[2]);})];},_1Xj=new T(function(){return B(unCStr("width"));}),_1Xk=new T(function(){return B(unCStr("height"));}),_1Xl=[0,200,0,0],_1Xm=new T(function(){return B(unCStr("style"));}),_1Xn=function(_1Xo,_1Xp,_1Xq,_1Xr,_1Xs,_1Xt,_1Xu,_1Xv,_1Xw){return function(_b3,_b4){return new F(function(){return _76(_1Xd,function(_1Xx,_1Xy,_){return new F(function(){return _76(_hx,function(_1Xz,_1XA,_){return new F(function(){return _76(function(_1XB,_){return [0,[0,function(_1XC,_){var _1XD=B(_1WW(_Y,_K,_1XC,_)),_1XE=_1XD,_1XF=B(A(_d,[_t,_1XE,_5Y,_1Xx,_])),_1XG=_1XF,_1XH=B(A(_d,[_t,_1XE,_1Xm,_1WU,_])),_1XI=_1XH,_1XJ=B(A(_d,[_t,_1XE,_1Xj,new T(function(){return B(A(_Ls,[_Lp,_Au,1.2*E(_1Xs)[1],_1g]));}),_])),_1XK=_1XJ,_1XL=B(A(_d,[_t,_1XE,_1Xk,new T(function(){return B(A(_Ls,[_Lp,_Au,1.2*E(_1Xt)[1],_1g]));}),_])),_1XM=_1XL;return _1XE;},_fJ],_1XB];},function(_1XN,_1XO,_){return [0,[0,function(_1XP,_){var _1XQ=jsFind(toJSStr(E(_1Xx))),_1XR=_1XQ,_1XS=E(_1XR);if(!_1XS[0]){return _1XP;}else{var _1XT=E(_1XS[1])[1],_1XU=jsHasCtx2D(_1XT),_1XV=_1XU;if(!E(_1XV)){return _1XP;}else{var _1XW=jsGetCtx2D(_1XT),_1XX=_1XW,_1XY=jsResetCanvas(_1XT),_1XZ=E(_1Xs),_1Y0=E(_1Xt),_1Y1=jsPushState(_1XX),_1Y2=jsTranslate(_1XX,0.1*_1XZ[1],0.1*_1Y0[1]),_1Y3=B(A(_1Sw,[_1Xq,_1Xr,[1,[0,new T(function(){return E(E(_1Xo)[1]);}),_1Xl],_1Xp],_1XZ,_1Y0,_1Xu,_1Xv,_1Xw,[0,_1XX],_])),_1Y4=_1Y3,_1Y5=jsPopState(_1XX);return _1XP;}}},_fJ],_1XO];},_1XA,_);});},_1Xy,_);});},_b3,_b4);});};},_1Y6=function(_){var _1Y7=B(A(_a,["(function() {return $(document).width();})",_])),_1Y8=_1Y7,_1Y9=B(A(_a,["(function() {return $(document).height();})",_])),_1Ya=_1Y9;return [0,new T(function(){return B(_Ts(_1Y8));}),new T(function(){return B(_Ts(_1Ya));})];},_1Yb=function(_1Yc,_){var _1Yd=B(_1Y6(_)),_1Ye=_1Yd;return [0,[0,_K,[1,_1Ye]],_1Yc];},_1Yf=new T(function(){return B(unCStr("col-md-12"));}),_1Yg=function(_1Yh){return E(E(_1Yh)[4]);},_1Yi=new T(function(){return B(unCStr("row"));}),_1Yj=function(_1Yk){return function(_b3,_b4){return new F(function(){return _0(new T(function(){return B(_aZ(_1Yk));}),_b3,_b4);});};},_1Yl=function(_1Ym,_1Yn,_1Yo){return function(_1Yp,_){var _1Yq=B(_c2(_Y,function(_1Yr,_){return new F(function(){return _lE([1,function(_1Ys,_){var _1Yt=B(_c2(_Y,function(_ij,_){return new F(function(){return _be(_1Yj,new T(function(){return [0,toJSStr(E(_1Yn))];}),_ij,_);});},_1Ys,_)),_1Yu=_1Yt,_1Yv=B(A(_d,[_t,_1Yu,_gO,new T(function(){return B(unAppCStr("col-md-",new T(function(){return B(_5M(0,E(_1Ym)[1],_1g));})));}),_])),_1Yw=_1Yv;return _1Yu;},[1,function(_1Yx,_){var _1Yy=B(_c2(_1Yj,new T(function(){return [0,toJSStr(E(_1Yo))];}),_1Yx,_)),_1Yz=_1Yy,_1YA=B(A(_d,[_t,_1Yz,_gO,new T(function(){return B(unAppCStr("col-md-",new T(function(){return B(_5M(0,12-E(_1Ym)[1]|0,_1g));})));}),_])),_1YB=_1YA;return _1Yz;},_1g]],_1Yr,_);});},_1Yp,_)),_1YC=_1Yq,_1YD=B(A(_d,[_t,_1YC,_gO,_1Yi,_])),_1YE=_1YD;return _1YC;};},_1YF=[0,0],_1YG=new T(function(){return B(unCStr("\u041f\u043e\u043a\u043e\u043b\u0435\u043d\u0438\u0435"));}),_1YH=new T(function(){return B(unCStr("\u0424\u0438\u0442\u043d\u0435\u0441"));}),_1YI=new T(function(){return B(unCStr("\u0422\u0435\u043a\u0443\u0449\u0438\u0439 \u0440\u0435\u0437\u0443\u043b\u044c\u0442\u0430\u0442"));}),_1YJ=[0,4],_1YK=new T(function(){return B(unCStr("\u041b\u0443\u0447\u0448\u0438\u0439 \u0444\u0438\u0442\u043d\u0435\u0441: "));}),_1YL=new T(function(){return B(unCStr("\u0422\u0435\u043a\u0443\u0449\u0438\u0439 \u043e\u0442\u0432\u0435\u0442: "));}),_1YM=new T(function(){return B(unCStr("\u0421\u0442\u043e\u0438\u043c\u043e\u0441\u0442\u044c \u043f\u0443\u0442\u0438: "));}),_1YN=[0,200],_1YO=new T(function(){return B(_Ls(_Lp,_Au,0));}),_1YP=new T(function(){return B(A(_1YO,[_1g]));}),_1YQ=[0,2],_1YR=[0,0],_1YS=new T(function(){return B(unCStr("h3"));}),_1YT=function(_1YU,_1YV,_1YW,_){var _1YX=jsCreateElem(toJSStr(E(_1YS))),_1YY=_1YX,_1YZ=jsAppendChild(_1YY,E(_1YW)[1]),_1Z0=[0,_1YY],_1Z1=B(A(_1YU,[_1YV,_1Z0,_])),_1Z2=_1Z1;return _1Z0;},_1Z3=new T(function(){return B(unCStr("panel panel-default"));}),_1Z4=new T(function(){return B(unCStr("panel-heading"));}),_1Z5=new T(function(){return B(unCStr("panel-title"));}),_1Z6=new T(function(){return B(unCStr("panel-body"));}),_1Z7=function(_1Z8,_1Z9){return function(_1Za,_){var _1Zb=B(_c2(_Y,function(_1Zc,_){return new F(function(){return _lE([1,function(_1Zd,_){var _1Ze=B(_c2(_Y,function(_1Zf,_){var _1Zg=B(_1YT(_1Yj,new T(function(){return [0,toJSStr(E(_1Z8))];}),_1Zf,_)),_1Zh=_1Zg,_1Zi=B(A(_d,[_t,_1Zh,_gO,_1Z5,_])),_1Zj=_1Zi;return _1Zh;},_1Zd,_)),_1Zk=_1Ze,_1Zl=B(A(_d,[_t,_1Zk,_gO,_1Z4,_])),_1Zm=_1Zl;return _1Zk;},[1,function(_1Zn,_){var _1Zo=B(_c2(_Y,_1Z9,_1Zn,_)),_1Zp=_1Zo,_1Zq=B(A(_d,[_t,_1Zp,_gO,_1Z6,_])),_1Zr=_1Zq;return _1Zp;},_1g]],_1Zc,_);});},_1Za,_)),_1Zs=_1Zb,_1Zt=B(A(_d,[_t,_1Zs,_gO,_1Z3,_])),_1Zu=_1Zt;return _1Zs;};},_1Zv=new T(function(){return B(unCStr("class"));}),_1Zw=function(_1Zx,_){return [0,[0,_K,[1,_1Zx]],_1Zx];},_1Zy=function(_1Zz,_1ZA,_){return [0,[0,_K,[1,new T(function(){return E(E(_1Zz)[4]);})]],_1ZA];},_1ZB=function(_ij,_){return new F(function(){return _76(_1Zw,_1Zy,_ij,_);});},_1ZC=function(_1ZD,_){var _1ZE=rMV(E(_9D)[1]),_1ZF=_1ZE;return [0,[0,_K,[1,_1ZF]],_1ZD];},_1ZG=[1,_c],_1ZH=[0,_K,_1ZG],_1ZI=function(_1ZJ,_){return [0,_1ZH,_1ZJ];},_1ZK=[0,_K,_5A],_1ZL=function(_1ZM,_){return [0,_1ZK,_1ZM];},_1ZN=function(_1ZO,_1ZP,_1ZQ,_){return new F(function(){return _76(_fU,function(_1ZR,_1ZS,_){return new F(function(){return _76(function(_1ZT,_){var _1ZU=E(_9D)[1],_1ZV=rMV(_1ZU),_1ZW=_1ZV,_=wMV(_1ZU,[1,_1ZR,_1ZW]);return [0,_1ZH,_1ZT];},function(_1ZX,_ij,_){return new F(function(){return (function(_ij,_){return new F(function(){return _76(_1ZB,function(_1ZY,_1ZZ,_){return new F(function(){return _76(function(_200,_){var _201=rMV(E(_9D)[1]),_202=_201;return [0,[0,_K,[1,new T(function(){return B(_qS(_ou,_1ZR,_202));})]],_200];},function(_203,_204,_){return new F(function(){return _76(new T(function(){return !E(_203)?E(_1ZI):function(_205,_){var _206=jsSetTimeout(E(_1ZO)[1],function(_){var _207=E(_9D)[1],_208=rMV(_207),_209=_208;if(!B(_qS(_ou,_1ZR,_209))){return _c;}else{var _20a=rMV(_207),_20b=_20a,_=wMV(_207,new T(function(){return B(_a6(function(_20c){return new F(function(){return _or(_20c,_1ZR);});},_20b));})),_20d=E(_1ZY),_20e=B(A(_20d[1],[_])),_20f=_20e,_20g=E(_20f);if(!_20g[0]){return _c;}else{var _20h=B(A(_20d[2],[_20g[1],_])),_20i=_20h;return _c;}}});return [0,_1ZH,_205];};}),function(_20j,_20k,_){return new F(function(){return _76(function(_ij,_){return new F(function(){return _76(_1ZC,function(_20l){return !B(_qS(_ou,_1ZR,_20l))?E(_1ZI):E(_1ZL);},_ij,_);});},function(_20m){return E(_1ZP);},_20k,_);});},_204,_);});},_1ZZ,_);});},_ij,_);});})(_ij,_);});},_1ZS,_);});},_1ZQ,_);});},_20n=function(_20o,_20p,_20q,_20r){var _20s=new T(function(){var _20t=E(_20p);if(!E(_20t[3])[0]){var _20u=E(_20q);}else{var _20u=[0,new T(function(){return B(_5B(E(_20q)[1],[1,[0,new T(function(){return [0,E(_20t[2])[1]];}),new T(function(){var _20v=E(_20t[4]);if(!_20v[0]){var _20w=E(_1YR);}else{var _20w=E(E(_20v[1])[1]);}return _20w;})],_1g]));})];}var _20x=_20u;return _20x;});return function(_b3,_b4){return new F(function(){return _76(_1Yb,function(_20y,_20z,_){var _20A=E(_20y);return new F(function(){return _76(function(_20B,_){var _20C=B(_76(new T(function(){return B(_1Xn(_20s,_1g,_1YG,_1YH,new T(function(){return [0,0.8*E(_20A[1])[1]];}),new T(function(){return [0,E(_20A[2])[1]*0.5];}),_1YF,_1YQ,_1g));}),function(_20D,_20E,_){return [0,[0,new T(function(){var _20F=new T(function(){var _20G=B(_91(function(_20H){return new F(function(){return _Sh(_20o,_20H);});},E(_20p)[3]));if(!_20G[0]){var _20I=[0];}else{var _20I=[1,E(_20G[1])[2]];}var _20J=_20I;return _20J;});return B(_1Z7(_1YI,function(_20K,_){return new F(function(){return _lE([1,new T(function(){return B(_1Yl(_1YJ,_1YK,new T(function(){var _20L=E(E(_20p)[4]);if(!_20L[0]){var _20M=E(_1YP);}else{var _20M=B(A(_Ls,[_Lp,_Au,E(E(_20L[1])[1])[1],_1g]));}var _20N=_20M;return _20N;},1)));}),[1,new T(function(){return B(_1Yl(_1YJ,_1YL,new T(function(){var _20O=E(_20F);return _20O[0]==0?[0]:B(_dr(_yx,_20O[1],_1g));},1)));}),[1,new T(function(){return B(_1Yl(_1YJ,_1YM,new T(function(){var _20P=E(_20F);if(!_20P[0]){var _20Q=[0];}else{var _20Q=B(_5M(0,B(_am(_20o,_20P[1])),_1g));}return _20Q;},1)));}),_1g]]],_20K,_);});}));}),_fJ],_20E];},_20B,_)),_20R=_20C,_20S=E(_20R),_20T=E(_20S[1]);return [0,[0,function(_20U,_){var _20V=B(_c2(_Y,_20T[1],_20U,_)),_20W=_20V,_20X=B(A(_d,[_t,_20W,_1Zv,_1Yf,_])),_20Y=_20X;return _20W;},_20T[2]],_20S[2]];},function(_20Z){return function(_9J,_){return new F(function(){return _76(function(_9J,_){return new F(function(){return _1ZN(_1YN,new T(function(){var _210=E(_20r);if(!_210[0]){var _211=function(_){var _212=E(_20p);return new F(function(){return _1uq(_1pA,[0,new T(function(){return B(_Ee(_20o));}),new T(function(){return B(_Ec(_20o));})],function(_20H){return new F(function(){return _Sh(_20o,_20H);});},new T(function(){return B(_1Yg(_20o));}),_212[1],_212[2],_212[3],_212[4],_212[5],_);});};}else{var _211=E(_210[1]);}var _213=_211,_214=function(_215,_){var _216=B(A(_213,[_])),_217=_216;return [0,[0,_K,[1,_217]],_215];},_218=E(_214);return _218;}),_9J,_);});},function(_219,_21a,_){return new F(function(){return _76(new T(function(){var _21b=E(_219);if(!_21b[0]){var _21c=function(_21d,_){return [0,[0,_K,[1,[0,_20p,[1,E(_21b[1])[2]]]]],_21d];};}else{var _21c=function(_21e,_){return [0,[0,_K,[1,[0,_21b[1],_5A]]],_21e];};}return _21c;}),function(_21f,_21g,_){var _21h=E(_21f);return [0,[0,_K,[1,[0,_21h[1],_20s,_21h[2]]]],_21g];},_21a,_);});},_9J,_);});};},_20z,_);});},_b3,_b4);});};},_21i=[0,_1g],_21j=new T(function(){return B(unCStr("col-md-4 col-md-offset-5"));}),_21k=function(_21l,_21m,_){var _21n=B(_c2(_Y,_21l,_21m,_)),_21o=_21n,_21p=B(A(_d,[_t,_21o,_1Zv,_21j,_])),_21q=_21p;return _21o;},_21r=new T(function(){return B(unCStr("row"));}),_21s=function(_21t,_21u,_){var _21v=B(_c2(_Y,_21t,_21u,_)),_21w=_21v,_21x=B(A(_d,[_t,_21w,_1Zv,_21r,_])),_21y=_21x;return _21w;},_21z=1,_21A=new T(function(){return B(unCStr("\u041d\u0430\u0447\u0430\u0442\u044c \u044d\u0432\u043e\u043b\u044e\u0446\u0438\u044e"));}),_21B=new T(function(){return B(_ig(_21z,_21A));}),_21C=new T(function(){return B(unCStr("btn btn-primary btn-lg"));}),_21D=[0,_1Zv,_21C],_21E=[1,_21D,_1g],_21F=function(_21G,_){var _21H=B(A(_21B,[_21G,_])),_21I=_21H,_21J=E(_21I),_21K=E(_21J[1]);return [0,[0,function(_9J,_){return new F(function(){return _21s(function(_9J,_){return new F(function(){return _21k(new T(function(){return B(_5f(_21K[1],_21E));}),_9J,_);});},_9J,_);});},_21K[2]],_21J[2]];},_21L=2,_21M=new T(function(){return B(unCStr("\u041e\u0441\u0442\u0430\u043d\u043e\u0432\u0438\u0442\u044c"));}),_21N=new T(function(){return B(_ig(_21L,_21M));}),_21O=0,_21P=new T(function(){return B(unCStr("\u041d\u0430\u0437\u0430\u0434"));}),_21Q=new T(function(){return B(_ig(_21O,_21P));}),_21R=function(_21S,_){var _21T=B(A(_21Q,[_21S,_])),_21U=_21T,_21V=E(_21U),_21W=E(_21V[1]),_21X=B(A(_21N,[_21V[2],_])),_21Y=_21X,_21Z=E(_21Y),_220=E(_21Z[1]);return [0,[0,function(_221,_){var _222=B(A(new T(function(){return B(_5f(_21W[1],_21E));}),[_221,_])),_223=_222,_224=B(A(new T(function(){return B(_5f(_220[1],_21E));}),[_221,_])),_225=_224;return _221;},new T(function(){var _226=E(_21W[2]);return _226[0]==0?E(_220[2]):E(_226);})],_21Z[2]];},_227=function(_228,_){var _229=B(_21R(_228,_)),_22a=_229,_22b=E(_22a),_22c=E(_22b[1]);return [0,[0,function(_9J,_){return new F(function(){return _21s(function(_9J,_){return new F(function(){return _21k(_22c[1],_9J,_);});},_9J,_);});},_22c[2]],_22b[2]];},_22d=new T(function(){return B(unCStr("\u041f\u0435\u0440\u0435\u0440\u0430\u0441\u0447\u0438\u0442\u0430\u0442\u044c"));}),_22e=new T(function(){return B(_ig(_21z,_22d));}),_22f=new T(function(){return B(unCStr("\u041d\u0430\u0447\u0430\u0442\u044c \u0441 \u043d\u0430\u0447\u0430\u043b\u0430"));}),_22g=new T(function(){return B(_ig(_21O,_22f));}),_22h=function(_22i,_){var _22j=B(A(_22g,[_22i,_])),_22k=_22j,_22l=E(_22k),_22m=E(_22l[1]),_22n=B(A(_22e,[_22l[2],_])),_22o=_22n,_22p=E(_22o),_22q=E(_22p[1]);return [0,[0,function(_22r,_){var _22s=B(A(new T(function(){return B(_5f(_22m[1],_21E));}),[_22r,_])),_22t=_22s,_22u=B(A(new T(function(){return B(_5f(_22q[1],_21E));}),[_22r,_])),_22v=_22u;return _22r;},new T(function(){var _22w=E(_22m[2]);return _22w[0]==0?E(_22q[2]):E(_22w);})],_22p[2]];},_22x=function(_22y,_){var _22z=B(_22h(_22y,_)),_22A=_22z,_22B=E(_22A),_22C=E(_22B[1]);return [0,[0,function(_9J,_){return new F(function(){return _21s(function(_9J,_){return new F(function(){return _21k(_22C[1],_9J,_);});},_9J,_);});},_22C[2]],_22B[2]];},_22D=new T(function(){return B(unAppCStr("invalid route in show state ",_7T));}),_22E=function(_9J,_){return new F(function(){return _0(_22D,_9J,_);});},_22F=new T(function(){return B(_5t(_22E));}),_22G=[0,_22F,_5A],_22H=new T(function(){return B(unCStr("row-fluid"));}),_22I=new T(function(){return B(unCStr("col-md-6"));}),_22J=new T(function(){return B(unCStr("\u0414\u0440\u0443\u0433\u0430\u044f \u0438\u043d\u0444\u043e\u0440\u043c\u0430\u0446\u0438\u044f"));}),_22K=[0,6],_22L=new T(function(){return B(unCStr("\u0420\u0435\u0437\u0443\u043b\u044c\u0442\u0430\u0442\u044b \u044d\u0432\u043e\u043b\u044e\u0446\u0438\u0438"));}),_22M=new T(function(){return B(unCStr("\u041b\u0443\u0447\u0448\u0435\u0435 \u0440\u0435\u0448\u0435\u043d\u0438\u0435: "));}),_22N=new T(function(){return B(unCStr("\u0412\u0445\u043e\u0434\u043d\u044b\u0435 \u0434\u0430\u043d\u043d\u044b\u0435"));}),_22O=new T(function(){return B(unCStr("\u041a\u043e\u043b\u0438\u0447\u0435\u0441\u0442\u0432\u043e \u0433\u043e\u0440\u043e\u0434\u043e\u0432:"));}),_22P=new T(function(){return B(unCStr("\u0414\u043b\u0438\u043d\u0430 \u0438\u043d\u0434\u0438\u0432\u0438\u0434\u043e\u0432:"));}),_22Q=new T(function(){return B(unCStr("\u041d\u0430\u0441\u0442\u0440\u043e\u0439\u043a\u0438 \u044d\u0432\u043e\u043b\u044e\u0446\u0438\u0438"));}),_22R=new T(function(){return B(unCStr("\u0428\u0430\u043d\u0441 \u043c\u0443\u0442\u0430\u0446\u0438\u0438: "));}),_22S=new T(function(){return B(unCStr("\u0427\u0430\u0441\u0442\u044c \u044d\u043b\u0438\u0442\u044b: "));}),_22T=new T(function(){return B(unCStr("\u041c\u0430\u043a\u0441\u0438\u043c\u0430\u043b\u044c\u043d\u043e\u0435 \u0447\u0438\u0441\u043b\u043e \u043f\u043e\u043a\u043e\u043b\u0435\u043d\u0438\u0439: "));}),_22U=new T(function(){return B(unCStr("\u041a\u043e\u043b-\u0432\u043e \u043f\u043e\u043f\u0443\u043b\u044f\u0446\u0438\u0439: "));}),_22V=new T(function(){return B(unCStr("\u041a\u043e\u043b-\u0432\u043e \u0438\u043d\u0434\u0438\u0432\u0438\u0434\u043e\u0432 \u0432 \u043f\u043e\u043f\u0443\u043b\u044f\u0446\u0438\u0438: "));}),_22W=new T(function(){return B(unCStr("\u041e\u0436\u0438\u0434\u0430\u0435\u043c\u043e\u0435 \u0437\u043d\u0430\u0447\u0435\u043d\u0438\u0435 \u0444\u0438\u0442\u043d\u0435\u0441\u0430: "));}),_22X=[0,_K,_5A],_22Y=function(_22Z,_230,_){return [0,_22X,_230];},_231=function(_232){return E(E(_232)[1]);},_233=function(_234,_235,_236){return function(_b3,_b4){return new F(function(){return _76(_1Yb,function(_237,_238,_){var _239=E(_237);return new F(function(){return _76(function(_23a,_){var _23b=B(A(new T(function(){return B(_1Xn(_235,_1g,_1YG,_1YH,new T(function(){return [0,E(_239[1])[1]*0.8];}),new T(function(){return [0,E(_239[2])[1]*0.8];}),_1YF,_1YQ,_1g));}),[_23a,_])),_23c=_23b,_23d=E(_23c),_23e=E(_23d[1]);return [0,[0,function(_23f,_){var _23g=B(_c2(_Y,function(_23h,_){var _23i=B(_c2(_Y,_23e[1],_23h,_)),_23j=_23i,_23k=B(A(_d,[_t,_23j,_1Zv,_1Yf,_])),_23l=_23k;return _23j;},_23f,_)),_23m=_23g,_23n=B(A(_d,[_t,_23m,_1Zv,_21r,_])),_23o=_23n;return _23m;},_23e[2]],_23d[2]];},function(_23p,_23q,_){var _23r=B(_76(function(_23s,_){return [0,[0,function(_23t,_){var _23u=B(_c2(_Y,function(_23v,_){return new F(function(){return _lE([1,function(_23w,_){var _23x=B(_c2(_Y,new T(function(){return B(_1Z7(_22N,function(_23y,_){return new F(function(){return _lE([1,new T(function(){return B(_1Yl(_22K,_22O,new T(function(){return B(_5M(0,E(E(_234)[2])[1],_1g));},1)));}),[1,new T(function(){return B(_1Yl(_22K,_22P,new T(function(){return B(_5M(0,E(E(_234)[3])[1],_1g));},1)));}),_1g]],_23y,_);});}));}),_23w,_)),_23z=_23x,_23A=B(A(_d,[_t,_23z,_1Zv,_22I,_])),_23B=_23A;return _23z;},[1,function(_23C,_){var _23D=B(_c2(_Y,new T(function(){var _23E=new T(function(){return E(E(_234)[4]);});return B(_1Z7(_22Q,function(_23F,_){return new F(function(){return _lE([1,new T(function(){return B(_1Yl(_22K,_22R,new T(function(){return B(A(_Ls,[_Lp,_Au,E(E(_23E)[1])[1],_1g]));},1)));}),[1,new T(function(){return B(_1Yl(_22K,_22S,new T(function(){return B(A(_Ls,[_Lp,_Au,E(E(_23E)[2])[1],_1g]));},1)));}),[1,new T(function(){return B(_1Yl(_22K,_22T,new T(function(){return B(_5M(0,E(E(_23E)[3])[1],_1g));},1)));}),[1,new T(function(){return B(_1Yl(_22K,_22U,new T(function(){return B(_5M(0,E(E(_23E)[4])[1],_1g));},1)));}),[1,new T(function(){return B(_1Yl(_22K,_22V,new T(function(){return B(_5M(0,E(E(_23E)[5])[1],_1g));},1)));}),[1,new T(function(){return B(_1Yl(_22K,_22W,new T(function(){var _23G=E(E(_23E)[6]);return _23G[0]==0?[0]:B(_LD(_23G[1]));},1)));}),_1g]]]]]],_23F,_);});}));}),_23C,_)),_23H=_23D,_23I=B(A(_d,[_t,_23H,_1Zv,_22I,_])),_23J=_23I;return _23H;},[1,function(_23K,_){var _23L=B(_c2(_Y,new T(function(){return B(_1Z7(_22L,function(_23M,_){return new F(function(){return _lE([1,new T(function(){return B(_1Yl(_22K,_1YK,new T(function(){return B(A(_Ls,[_Lp,_Au,E(E(_236)[3])[1],_1g]));},1)));}),[1,new T(function(){return B(_1Yl(_22K,_22M,new T(function(){return B(_dr(_yx,E(_236)[1],_1g));},1)));}),_1g]],_23M,_);});}));}),_23K,_)),_23N=_23L,_23O=B(A(_d,[_t,_23N,_1Zv,_22I,_])),_23P=_23O;return _23N;},[1,function(_23Q,_){var _23R=B(_c2(_Y,new T(function(){return B(_1Z7(_22J,function(_23S,_){return new F(function(){return _lE([1,new T(function(){return B(_1Yl(_22K,_1YM,new T(function(){return B(_5M(0,B(_am(_234,B(_231(_236)))),_1g));},1)));}),_1g],_23S,_);});}));}),_23Q,_)),_23T=_23R,_23U=B(A(_d,[_t,_23T,_1Zv,_22I,_])),_23V=_23U;return _23T;},_1g]]]],_23v,_);});},_23t,_)),_23W=_23u,_23X=B(A(_d,[_t,_23W,_1Zv,_22H,_])),_23Y=_23X;return _23W;},_fJ],_23s];},_22Y,_23q,_)),_23Z=_23r,_240=E(_23Z),_241=E(_240[1]);return [0,[0,function(_242,_){var _243=B(_c2(_Y,_241[1],_242,_)),_244=_243,_245=B(A(_d,[_t,_244,_1Zv,_21r,_])),_246=_245;return _244;},_241[2]],_240[2]];},_238,_);});},_b3,_b4);});};},_247=function(_248){var _249=E(_248);switch(_249[0]){case 0:var _24a=_249[1];return function(_b3,_b4){return new F(function(){return _76(function(_9J,_){return new F(function(){return _aH(new T(function(){return B(_OB(_24a));}),_21F,_9J,_);});},function(_24b,_24c,_){var _24d=E(_24b);if(!_24d[0]){return [0,[0,_K,[1,[0,_24d[1]]]],_24c];}else{var _24e=E(_24d[1]);return _24e==1?B(_76(_9u,function(_24f,_24g,_){return [0,[0,_K,[1,[1,_24a,_21i,_24f,_5A]]],_24g];},_24c,_)):[0,[0,new T(function(){return B(_5t(function(_9J,_){return new F(function(){return _0(new T(function(){return B(unAppCStr("invalid route in config state ",new T(function(){return E(_24e)==0?E(_7U):E(_7T);})));}),_9J,_);});}));}),_5A],_24c];}},_b3,_b4);});};case 1:var _24h=_249[1],_24i=_249[2],_24j=_249[3],_24k=function(_20H){return new F(function(){return _Sh(_24h,_20H);});};return function(_b3,_b4){return new F(function(){return _76(function(_9J,_){return new F(function(){return _aH(new T(function(){return B(_20n(_24h,_24j,_24i,_249[4]));}),_227,_9J,_);});},function(_24l,_24m,_){var _24n=E(_24l);if(!_24n[0]){var _24o=E(_24n[1]),_24p=_24o[2];return [0,[0,_K,[1,new T(function(){var _24q=E(_24o[1]);return !E(_24q[1])?[1,_24h,_24p,_24q,_24o[3]]:[2,_24h,_24p,new T(function(){var _24r=B(_91(_24k,_24q[3]));if(!_24r[0]){var _24s=E(_gQ);}else{var _24t=E(_24r[1]),_24u=_24t[2],_24s=[0,_24u,new T(function(){return B(_ay(_24h,_24u));}),_24t[1]];}return _24s;})];})]],_24m];}else{return new F(function(){return _76(_9E,function(_24v){return E(new T(function(){switch(E(_24n[1])){case 0:var _24w=function(_24x,_){return [0,[0,_K,[1,[0,_24h]]],_24x];};break;case 1:var _24w=E(_9M);break;default:var _24w=function(_24y,_){return [0,[0,_K,[1,[2,_24h,_24i,new T(function(){var _24z=B(_91(_24k,E(_24j)[3]));if(!_24z[0]){var _24A=E(_gQ);}else{var _24B=E(_24z[1]),_24C=_24B[2],_24A=[0,_24C,new T(function(){return B(_ay(_24h,_24C));}),_24B[1]];}var _24D=_24A;return _24D;})]]],_24y];};}return _24w;}));},_24m,_);});}},_b3,_b4);});};default:var _24E=_249[1];return function(_b3,_b4){return new F(function(){return _76(function(_9J,_){return new F(function(){return _aH(new T(function(){return B(_233(_24E,_249[2],_249[3]));}),_22x,_9J,_);});},function(_24F,_24G,_){var _24H=E(_24F);if(!_24H[0]){return [0,[0,_K,[1,_249]],_24G];}else{switch(E(_24H[1])){case 0:return [0,[0,_K,[1,[0,_24E]]],_24G];case 1:return new F(function(){return _76(_9u,function(_24I,_24J,_){return [0,[0,_K,[1,[1,_24E,_21i,_24I,_5A]]],_24J];},_24G,_);});break;default:return [0,_22G,_24G];}}},_b3,_b4);});};}},_24K=[0,35],_24L=new T(function(){return B(unCStr("id"));}),_24M=function(_24N,_24O,_24P,_){var _24Q=B(A(_5T,[_24P,_24P,_])),_24R=_24Q,_24S=new T(function(){return E(E(_24R)[1]);}),_24T=function(_24U,_24V){return function(_b3,_b4){return new F(function(){return _76(function(_24W,_){return new F(function(){return _1z([1,_24K,_24U],_1a,new T(function(){return B(A(_24O,[_24V]));}),_24W,_);});},function(_24X){return new F(function(){return _24T(_24U,_24X);});},_b3,_b4);});};},_24Y=B(A(_24T,[_24S,_24N,new T(function(){return E(E(_24R)[2]);}),_])),_24Z=_24Y,_250=E(_24Z),_251=E(_250[1]);return [0,[0,function(_252,_){var _253=B(_6W(_Y,_K,_252,_)),_254=_253,_255=B(A(_d,[_t,_254,_24L,_24S,_])),_256=_255,_257=B(A(_251[1],[_252,_])),_258=_257;return _252;},_251[2]],_250[2]];},_259=function(_25a,_){return new F(function(){return _24M(_4Z,_247,_25a,_);});},_25b=[0,1000],_25c=function(_25d,_){return new F(function(){return _1ZN(_25b,_259,_25d,_);});},_25e=new T(function(){return B(unCStr("main-content"));}),_25f=[1,_24K,_25e],_25g=function(_25h,_){return new F(function(){return _1z(_25f,_1a,_25c,_25h,_);});},_25i=new T(function(){return B(unCStr("./bootstrap.min.js"));}),_25j=new T(function(){return B(unCStr("./jquery-1.11.2.min.js"));}),_25k=[0,125],_25l=[1,_25k,_1g],_25m=new T(function(){return B(unAppCStr("  justify-content: center; \n",_25l));}),_25n=new T(function(){return B(unAppCStr("  align-items: center;\n",_25m));}),_25o=new T(function(){return B(unAppCStr("  display: flex;\n",_25n));}),_25p=new T(function(){return B(unAppCStr(".vertical-align > [class*=\" col-\"] {\n",_25o));}),_25q=new T(function(){return B(unAppCStr(".vertical-align > [class^=\"col-\"],\n",_25p));}),_25r=new T(function(){return B(unAppCStr("}\n",_25q));}),_25s=new T(function(){return B(unAppCStr("  flex-direction: row;\n",_25r));}),_25t=new T(function(){return B(unAppCStr("  display: flex;\n",_25s));}),_25u=new T(function(){return B(unAppCStr(".vertical-align {\n",_25t));}),_25v=new T(function(){return B(unCStr("var mouse = {x: 0, y: 0};\ndocument.addEventListener(\'mousemove\', function(e){\nmouse.x = e.clientX || e.pageX;\nmouse.y = e.clientY || e.pageY\n}, false);"));}),_25w=new T(function(){return B(unCStr("(function(){return document.body;})"));}),_25x=function(_25y,_){var _25z=B(A(_a,[toJSStr(E(_25w)),_])),_25A=_25z,_25B=E(_6x)[1],_25C=takeMVar(_25B),_25D=_25C,_25E=B(A(_25y,[_25D,_])),_25F=_25E,_25G=E(_25F),_25H=E(_25G[1]),_=putMVar(_25B,_25G[2]),_25I=B(A(_25H[1],[[0,_25A],_])),_25J=_25I;return _25H[2];},_25K=function(_){var _25L=B(_B(_19,_)),_25M=_25L,_25N=B(_B(_18,_)),_25O=_25N,_25P=toJSStr(E(_l)),_25Q=B(A(_a,[_25P,_])),_25R=_25Q,_25S=B(_5n(_17,[0,_25R],_)),_25T=_25S,_25U=B(_0(_25u,_25T,_)),_25V=_25U,_25W=B(_10(_25j,_)),_25X=_25W,_25Y=B(_10(_25i,_)),_25Z=_25Y,_260=B(A(_a,[_25P,_])),_261=_260,_262=B(_N(_0,_25v,[0,_261],_)),_263=_262;return new F(function(){return _25x(_25g,_);});},_264=function(_){return new F(function(){return _25K(_);});};
var hasteMain = function() {B(A(_264, [0]));};window.onload = hasteMain;