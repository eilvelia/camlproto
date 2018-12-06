function _isNode () {
  return Boolean(
    typeof process !== 'undefined'
    && process.versions
    && process.versions.node
  )
}

var _crypto = _isNode() ? require('crypto') : { 'ENV': 'Browser' }

var _js_sha1_node = {
  init: function () { return _crypto.createHash('sha1') },
  feed: function (t, buf) { t.update(buf.data) },
  get: function (t) { return t.digest().buffer }
}

// TODO: use https://github.com/digitalbazaar/forge
//  or https://github.com/brix/crypto-js

var _js_sha1_browser = { // TODO:
  init: function () { console.log('sha1 browser init'); return 'a' },
  feed: function (t, buf) { console.log(buf.data) },
  get: function (t) { return new Uint8Array([1,2]).buffer }
}

var _js_sha256_node = {
  init: function () { return _crypto.createHash('sha256') },
  feed: function (t, buf) { t.update(buf.data) },
  get: function (t) { return t.digest().buffer }
}

var _js_sha256_browser = { // TODO:
  init: function () { console.log('sha256 browser init'); return 'a' },
  feed: function (t, buf) { console.log(buf.data) },
  get: function (t) { return new Uint8Array([1,2,3]).buffer }
}

var _js_aes_node = {
  ecbCreateKey: function (key) { return key },
  ecbEncrypt: function (key, buf) {
    var cipherName = key.data.length === 32 ? 'aes-256-ecb' : 'aes-128-ecb'
    var cipher = _crypto.createCipheriv(cipherName, key.data, '')
    cipher.setAutoPadding(false)
    var output = cipher.update(buf.data)
    cipher.end()
    return output.buffer
  },
  ecbDecrypt: function (key, buf) {
    var decipherName = key.data.length === 32 ? 'aes-256-ecb' : 'aes-128-ecb'
    var decipher = _crypto.createDecipheriv(decipherName, key.data, '')
    decipher.setAutoPadding(false)
    var output = decipher.update(buf.data)
    decipher.end()
    return output.buffer
  },
}

var _js_aes_browser = { // TODO:
  ecbCreateKey: function (key) { return key },
  ecbEncrypt: function (key, buf) { return new Uint8Array([1,2,3]).buffer },
  ecbDecrypt: function (key, buf) { return new Uint8Array([3,2,1]).buffer },
}

var _js_secure_rand_node = {
  rand: function (size) { return _crypto.randomBytes(size).buffer }
}

var _js_secure_rand_browser = { // TODO:
  rand: function (size) { console.log('secure rand'); return new Uint8Array([1]).buffer }
}

var js_sha1 = _isNode() ? _js_sha1_node : _js_sha1_browser
var js_sha256 = _isNode() ? _js_sha256_node : _js_sha256_browser
var js_aes = _isNode() ? _js_aes_node : _js_aes_browser
var js_secure_rand = _isNode() ? _js_secure_rand_node : _js_secure_rand_browser

// ---

function arrayBufferFromHex (hexString) {
  var len = hexString.length
  var start = 0
  var bytes = new Uint8Array((len + len % 2) / 2)

  if (len % 2 === 1) {
    bytes[0] = parseInt(hexString.charAt(0), 16)
    start++
  }

  var i = start
  var bytei = start
  for (; i < len; i += 2, bytei++) {
    bytes[bytei] = parseInt(hexString.substr(i, 2), 16)
  }

  return bytes
}

function arrayBufferToHex (buf) {
  var bytes = new Uint8Array(buf)
  var arr = []
  var i = 0
  for (; i < bytes.length; i++) {
    arr.push((bytes[i] < 16 ? '0' : '') + (bytes[i] || 0).toString(16))
  }
  return arr.join('')
}

function bigInt2ArrayBuffer (bigint) {
  // try {
  // console.log('!!! bigInt2ArrayBuffer - start')
  var str = bigInt2str(bigint, 16)
  var out = arrayBufferFromHex(str)
  // console.log('!!! bigInt2ArrayBuffer - end')
  return out
  // } catch (e) { console.error(e); throw e }
}

function arrayBuffer2bigInt (buf) {
  // console.log('!!! arrayBuffer2bigInt - start')
  var str = arrayBufferToHex(buf)
  return str2bigInt(str, 16)
}

function camlBigarray2bigInt (arr) {
  return arrayBuffer2bigInt(arr.data.buffer)
}
