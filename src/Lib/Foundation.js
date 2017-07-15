"use strict";
//requires web3; truffle-contract; Foundation configs

var Foundation;

exports.initImpl = function(dummyVal) {
    return function() {
        Foundation = TruffleContract(foundationConfig);
        Foundation.setProvider(web3.currentProvider);
    };
};

exports.resolveToAddrImpl = function(callback) {
    return function(foundationId) {
        return function() {
            Foundation.deployed().then(function(instance) {
                return instance.resolveToAddresses.call(foundationId);
            }).then(function(res) {
                callback(res.valueOf())();
            });
        };
    };
};

exports.resolveToNameImpl = function(callback) {
    return function(addr) {
        return function() {
            Foundation.deployed().then(function(instance) {
                return instance.resolveToName.call(addr);
            }).then(function(res) {
                callback(b2s(res.valueOf()))();
            });
        };
    };
};


var bytes2addrList = function(byteArrayList) {
    var l = [];
    for ( var i=0; i < byteArrayList.length; i++) {
        l.push(b2s(byteArrayList[i]));
    }
    return l;
};

var b2s = function(bytes) {
    var s = "";
    for(var i=2; i<bytes.length; i+=2) {
        var num = parseInt(bytes.substring(i, i+2), 16);
        if (num == 0) break;
        var char = String.fromCharCode(num);
        s += char;
    }
    return s;
};
