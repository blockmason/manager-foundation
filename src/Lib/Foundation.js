"use strict";
//requires web3; truffle-contract; Foundation configs, BigNumber.js

var Foundation;
var foundationAbi = web3.eth.contract(foundationConfig.abi);
var foundationContract = foundationAbi.at(foundationConfig.address);

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

exports.areSameIdImpl = function(callback) {
    return function(addr1) {
        return function(addr2) {
            return function() {
                Foundation.deployed().then(function(instance) {
                    return instance.areSameId.call(addr1, addr2);
                }).then(function(res) {
                    callback(b2s(res.valueOf()))();
                });
            };
        };
    };
};


exports.createIdImpl = function(foundationId) {
    return function() {
        Foundation.deployed().then(function(instance) {
            return instance.createId(foundationId);
        });
    };
};

exports.sentPendingImpl = function(callback) {
    return function(foundationId) {
        return function() {
            Foundation.deployed().then(function(instance) {
                return instance.sentPending.call(foundationId);
            }).then(function(r) {
                callback(r.valueOf())();
            });
        };
    };
};

exports.todoPendingImpl = function(callback) {
    return function(addr) {
        return function() {
            Foundation.deployed().then(function(instance) {
                return instance.todoPending.call(addr);
            }).then(function(r) {
                callback(b2s(r.valueOf()))();
            });
        };
    };
};

exports.addPendingUnificationImpl = function(addr) {
    return function() {
        Foundation.deployed().then(function(instance) {
            return instance.addPendingUnification(addr);
        });
    };
};

exports.confirmPendingUnificationImpl = function(foundationId) {
    return function() {
        Foundation.deployed().then(function(instance) {
            return instance.confirmPendingUnification(foundationId);
        });
    };
};

exports.deleteAddrImpl = function(addr) {
    return function() {
        Foundation.deployed().then(function(instance) {
            return instance.deleteAddr(addr);
        });
    };
};

exports.depositWeiImpl = function(foundationId) {
    return function(weiAmountStr) {
        return function() {
            var w = new BigNumber(weiAmountStr);
            Foundation.deployed().then(function(instance) {
                return instance.depositWei({value: w});
            });
        };
    };
};

exports.withdrawDepositImpl = function(foundationId) {
    return function() {
        Foundation.deployed().then(function(instance) {
            return instance.withdrawDeposit(foundationId);
        });
    };
};

exports.getDepositWeiImpl = function(callback) {
    return function(foundationId) {
        return function() {
            Foundation.deployed().then(function(instance) {
                return instance.getDepositWei.call(foundationId);
            }).then(function(r) {
                callback(r.valueOf())();
            });
        };
    };
};

exports.expirationDateImpl = function(callback) {
    return function(foundationId) {
        return function() {
            Foundation.deployed().then(function(instance) {
                return instance.getExpirationDate.call(foundationId);
            }).then(function(r) {
                callback(parseInt(r.valueOf()))();
            });
        };
    };
};

/* ********** helpers ********** */

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

/* ************************ */
exports.printTransactionImpl = function(unit) {
    return function() {
        /*
        web3.eth.getTransaction("0x7a406952d4bcc5f600a397f4101fe64b4a6116a08a4a8f7d79ded9ecbbde51c3", function(error, result) {
            console.log(result);
            console.log(foundationContract);
        });
         */
        var data = foundationContract.addPendingUnification.getData("0xB07cd7De89Fa764301b6cC5f41eCd1497b72a475");
        web3.eth.sendTransaction({to: "0x694a92520101d8f78a7aba2578380628565e3621",
                                  from: web3.eth.accounts[0],
                                  data: data }, function(err, res) {
                                      console.log(res);
                                  });
    };
};
