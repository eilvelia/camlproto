// WIP
// TODO:

var _net = require('net')

// CRC32 taken from
// https://github.com/SheetJS/js-crc32/blob/02caecf54f5f8cd9fbcbc295cbf3d6553db97e58/crc32.js

function _signedCrcTable() {
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

var _crcT = _signedCrcTable();

function crc32Buf8(buf, seed) {
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

function crc32Buf(buf, seed) {
	if(buf.length > 10000) return crc32Buf8(buf, seed);
	var C = seed ^ -1, L = buf.length - 3;
	for(var i = 0; i < L;) {
		C = (C>>>8) ^ _crcT[(C^buf[i++])&0xFF];
		C = (C>>>8) ^ _crcT[(C^buf[i++])&0xFF];
		C = (C>>>8) ^ _crcT[(C^buf[i++])&0xFF];
		C = (C>>>8) ^ _crcT[(C^buf[i++])&0xFF];
	}
	while(i < L+3) C = (C>>>8) ^ _crcT[(C^buf[i++])&0xFF];
	return C ^ -1;
}

/** Returns Uint8Array with length 4 */
function jsCrc32 (buf) {
  var int = crc32Buf(buf)
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

function _read (stream, n) {
  var buf = stream.read(n)

  if (buf) return Promise.resolve(buf)

  return new Promise(function (resolve, reject) {
    stream.once('readable', function () {
      var buf1 = stream.read(n)
      if (buf1) return resolve(buf1)
      console.error('js tcp full: End Of File')
      reject(new Error('End Of File'))
    })
  })
}

function _nodeBufferToArrayBuffer (data) {
  return data.buffer.slice(data.byteOffset, data.byteOffset + data.byteLength)
}

// TCP Full for Node.js.

this.js_tcp_full = {
  create: function (address, port, cb) {
    var socket = _net.createConnection(port, address, function () {
      console.log('js_tcp_full connected')
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
    var checksum = jsCrc32(forChecksum)
    bufUint8.set(checksum, len - 4)

    console.log('socket.write', bufUint8.length)
    t.socket.write(bufUint8, function () {
      console.log('js send: socket.write callback')
      t.seqNo++
      cb()
    })
    } catch (e) { console.error(e); throw e }
  },
  receive: function (t, cb) {
    console.log('receive start', 'cl_seqNo:', t.seqNo)

    _read(t.socket, 8).then(function (lenSeq) {
    console.log('received lenSeq', lenSeq.length)

    var lenSeqArr = new Uint32Array(_nodeBufferToArrayBuffer(lenSeq))
    var len = lenSeqArr[0]
    var seqNo = lenSeqArr[1]

    var bodyLen = len - 12

    console.log('received', 'len', len, 'sv_seqNo', seqNo)

    return _read(t.socket, bodyLen).then(function (body) {
    console.log('received body', body.length)

    if (body.length < bodyLen) {
      console.error('Invalid body length')
      throw new Error('TCP Full: Invalid len ' + body.length)
    }

    return _read(t.socket, 4).then(function (givenChecksum) {
    console.log('received checksum', givenChecksum)

    var givenChecksumArr = new Uint8Array(givenChecksum)

    var forChecksum = Buffer.concat([lenSeq, body])
    console.log('forChecksum len', forChecksum.length)
    var calcChecksumArr = jsCrc32(forChecksum)

    if (!_eq(calcChecksumArr, givenChecksumArr)) {
      console.error('Invalid checksum', calcChecksumArr, givenChecksumArr)
      throw new Error('TCP Full: Invalid checksum')
    }

    if (seqNo + 1 !== t.seqNo) {
      console.error('Incorrect sequence number', 'cl', t.seqNo, 'sv', seqNo)
      throw new Error('TCP Full: Incorrect sequence number')
    }

    cb(_nodeBufferToArrayBuffer(body))
    })
    })
    })//.catch(function (e) {}) // TODO:
  }
}
