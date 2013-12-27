var fs = require('fs');
var request = require('request');
var hex64 = require('./hex64');

request.get('http://blog.differentpla.net/post/all', function(err, res, body) {
  console.log(body);
  var all = JSON.parse(body);
  all.forEach(function(x) {
    var prefix = x.created_at.replace(/T.*/, '');
var title = x.title.replace(/[ |&;$%@"<>?()+,"':\/.]/g, '-');
title = title.replace(/-{2,}/g, '-');
var filename = prefix + '-' + title + '.md';
console.log(filename);

var front_matter = '---' + '\n';
front_matter += 'layout: post' + '\n';
front_matter += 'title: "' + x.title + '"\n';
front_matter += 'date: ' + x.created_at + '\n';
if (x.tags) {
  if (typeof(x.tags) == 'string') {
    front_matter += 'tags: ' + x.tags.split(',').join(' ') + '\n';
  } else {
    front_matter += 'tags: ' + x.tags.join(' ') + '\n';
  }
}
front_matter += 'alias: ' + '/post/' + hex64.encode(x._id) + '/' + title.toLowerCase() + '\n';
front_matter += '---' + '\n';
front_matter += '\n';

var content = front_matter + x.markdown;
content = content.replace(/\\r/g, '');
fs.writeFileSync(filename, content);
  });
});
