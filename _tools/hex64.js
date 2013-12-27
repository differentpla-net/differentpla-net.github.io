module.exports = exports = {
  encode : encode,
  decode : decode
};

function encode(str) {
  var buffer = new Buffer(str.toString(), 'hex');
  var temp = buffer.toString('base64');
  return temp.replace(/\+/g, '-').replace(/\//g, '_');
}

function decode(str) {
  var temp = str.toString().replace(/-/g, '+').replace(/_/g, '/');
  var buffer = new Buffer(temp, 'base64');
  return buffer.toString('hex');
}
