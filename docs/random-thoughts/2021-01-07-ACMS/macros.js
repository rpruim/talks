remark.macros.scale = function(w) {
  var url = this;
  return '<img src="' + url + '" style="width: ' + w + '" />';
};

remark.macros.center = function(w = '80%') {
  var url = this;
  return '<img src="' + url + '" style="width: ' + w + '; display: bock; margin: 10px auto 20px" />';
};