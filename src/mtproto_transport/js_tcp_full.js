// WIP
// TODO:

var _net = require('net')

// CRC32 taken from
// https://github.com/SheetJS/js-crc32/blob/02caecf54f5f8cd9fbcbc295cbf3d6553db97e58/crc32.js

function _signed_crc_table() {
	var c = 0, table = new Array(256);

	for(var n =0; n != 256; ++n){
		c = n;
		c = ((c&1) ? (-306674912 ^ (c >>> 1)) : (c >>> 1));
		c = ((c&1) ? (-306674912 ^ (c >>> 1)) : (c >>> 1));
		c = ((c&1) ? (-306674912 ^ (c >>> 1)) : (c >>> 1));
		c = ((c&1) ? (-306674912 ^ (c >>> 1)) : (c >>> 1));
		c = ((c&1) ? (-306674912 ^ (c >>> 1)) : (c >>> 1));
		c = ((c&1) ? (-306674912 ^ (c >>> 1)) : (c >>> 1));
		c = ((c&1) ? (-306674912 ^ (c >>> 1)) : (c >>> 1));
		c = ((c&1) ? (-306674912 ^ (c >>> 1)) : (c >>> 1));
		table[n] = c;
	}

	return typeof Int32Array !== 'undefined' ? new Int32Array(table) : table;
}

var _crc_T = _signed_crc_table();

function crc32_buf_8(buf, seed) {
	var C = seed ^ -1, L = buf.length - 7;
	for(var i = 0; i < L;) {
		C = (C>>>8) ^ T[(C^buf[i++])&0xFF];
		C = (C>>>8) ^ T[(C^buf[i++])&0xFF];
		C = (C>>>8) ^ T[(C^buf[i++])&0xFF];
		C = (C>>>8) ^ T[(C^buf[i++])&0xFF];
		C = (C>>>8) ^ T[(C^buf[i++])&0xFF];
		C = (C>>>8) ^ T[(C^buf[i++])&0xFF];
		C = (C>>>8) ^ T[(C^buf[i++])&0xFF];
		C = (C>>>8) ^ T[(C^buf[i++])&0xFF];
	}
	while(i < L+7) C = (C>>>8) ^ T[(C^buf[i++])&0xFF];
	return C ^ -1;
}

function crc32_buf(buf, seed) {
	if(buf.length > 10000) return crc32_buf_8(buf, seed);
	var C = seed ^ -1, L = buf.length - 3;
	for(var i = 0; i < L;) {
		C = (C>>>8) ^ _crc_T[(C^buf[i++])&0xFF];
		C = (C>>>8) ^ _crc_T[(C^buf[i++])&0xFF];
		C = (C>>>8) ^ _crc_T[(C^buf[i++])&0xFF];
		C = (C>>>8) ^ _crc_T[(C^buf[i++])&0xFF];
	}
	while(i < L+3) C = (C>>>8) ^ _crc_T[(C^buf[i++])&0xFF];
	return C ^ -1;
}

/** Returns Uint8Array with length 4 */
function js_crc32 (buf) {
  var int = crc32_buf(buf)
  var uint32Arr = new Uint32Array([int])
  var uint8Arr = new Uint8Array(uint32Arr.buffer)
  return uint8Arr
}

function _eq (arr1, arr2) {
  for (var i = 0; i < arr1.length; i++) {
    if (arr1[i] !== arr2[i])
      return false
  }
  return arr1.length === arr2.length
}

// TCP Full for Node.js.

var js_tcp_full = {
  create: function (address, port, cb) {
    var socket = _net.createConnection(port, address, function () {
      console.log('js_tcp_full connected') // TODO:
      cb({ socket: socket, seqNo: 0 })
    })
  },
  send: function (t, packet_, cb) {
    var packet = packet_.data // Uint8Array
    try { // On release builds exceptions are silently ignored
    console.log('js send', t.seqNo, packet.length)

    var len = 4 * 3 + packet.length
    var buf = new ArrayBuffer(len)
    var bufUint8 = new Uint8Array(buf)

    var lenSeqBytes = new Uint32Array(buf, 0, 2)
    lenSeqBytes.set([len, t.seqNo])

    bufUint8.set(packet, 8)

    var forChecksum = bufUint8.slice(0, len - 4)
    var checksum = js_crc32(forChecksum)
    bufUint8.set(checksum, len - 4)
    console.log('socket.write')
    t.socket.write(bufUint8, function () {
      console.log('js send: socket.write callback')
      t.seqNo++
      cb()
    }) } catch (e) { console.error(e)}
  }, // TODO: buffer
  receive: function (t, cb) { // TODO:
    try { console.log('receive', 'seqNo:', t.seqNo)
    t.socket.once('data', function (data) {
      console.log('received data', data.length)
      var arrBuf =
        data.buffer.slice(data.byteOffset, data.byteOffset + data.byteLength)
      // var lenSeqArr = new Uint32Array(arrBuf.slice(0, 8))
      var lenSeqArr = new Uint32Array(data.slice(0, 8))
      var len = lenSeqArr[0]
      var seqNo = lenSeqArr[1]
      console.log('len', len, 'seqNo', seqNo)
      if (data.length < len) {
        console.error('Invalid data length')
        throw new Error('TCP Full: Invalid len ' + data.length)
      }

      var body = new Uint8Array(arrBuf, 8, len - 12)

      // TODO: checksum check doesn't work ._.

      // // console.log(new Uint8Array(data))
      // var forChecksum = data.slice(0, len - 12)
      // console.log('ch len', forChecksum.length)
      // var calcChecksum = js_crc32(forChecksum)
      // var givenChecksum = new Uint8Array(data.slice(len - 12, len - 8))
      // if (!_eq(calcChecksum, givenChecksum)) {
      //   console.error('Invalid checksum', calcChecksum, givenChecksum)
      //   throw new Error('TCP Full: Invalid checksum')
      // }

      if (seqNo + 1 !== t.seqNo) {
        console.error('Incorrect sequence number', seqNo, t.seqNo)
        throw new Error('TCP Full: Incorrect sequence number')
      }

      // console.log(data.length, body.length)
      cb(body.buffer.slice(body.byteOffset, body.byteOffset + body.byteLength))
    }) } catch (e) { console.error(e) }
  }
}
