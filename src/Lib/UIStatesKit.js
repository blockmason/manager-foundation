exports.toggleLoadingImpl = function(selector) {
  return function() {
    if (selector) {
      document.querySelectorAll(selector).forEach( function(el){
        el.classList.toggle('loading');
      });
    }
  }
}

exports.turnOnLoadingImpl = function(selector) {
  return function() {
    if (selector) {
      document.querySelectorAll(selector).forEach( function(el){
        el.classList.add('loading');
      });
    }
  }
}

exports.toggleErrorImpl = function(selector) {
  return function() {
      if (selector) {
        document.querySelectorAll(selector).forEach( function(el){
          el.classList.toggle('error');
        });
    }
  }
}

exports.clearAllErrorsImpl = function(parentSelector) {
  return function() {
      var parent = parentSelector ? document.querySelector(parentSelector) : document
      parent.querySelectorAll('.error').forEach( function(el){
      el.classList.remove('error');
    });
  }
}

exports.clearAllLoadingImpl = function(parentSelector) {
  return function() {
      var parent = parentSelector ? document.querySelector(parentSelector) : document
      parent.querySelectorAll('.loading').forEach( function(el){
      el.classList.remove('loading');
    });
  }
}
