"use strict";
//requires web3; truffle-contract; Foundation configs, BigNumber.js

var foundationAbi;
var fContract;
var fAddress;
//var myAddress;

exports.initImpl = function(dummyVal) {
    return function() {
        foundationAbi = web3.eth.contract(foundationConfig.abi);
        fContract = foundationAbi.at(foundationConfig.address);
        fAddress = foundationConfig.address;
//        myAddress = web3.eth.accounts[0];
    };
};

exports.resolveToAddrImpl = function(callback) {
    return function(foundationId) {
        return function() {
            fContract.resolveToAddresses(foundationId, function(e, r) {
                if ( !e ) callback(r)();
                else      console.error(e);
            });
        };
    };
};

exports.resolveToNameImpl = function(callback) {
    return function(addr) {
        return function() {
            fContract.resolveToName(addr, function(e, r) {
                if ( !e ) callback(b2s(r))();
                else      console.error(e);
            });
        };
    };
};

exports.areSameIdImpl = function(callback) {
    return function(addr1) {
        return function(addr2) {
            return function() {
                fContract.areSameId(addr1, addr2, function(e, r) {
                    if ( !e ) callback(b2s(r))();
                    else      console.error(e);
                });
            };
        };
    };
};

exports.createIdImpl = function(callback) {
    return function(foundationId) {
        return function() {
            var data = fContract.createId.getData(foundationId);
            sendFoundationTx(data, 0, callback);
        };
    };
};

exports.sentPendingImpl = function(callback) {
    return function(foundationId) {
        return function() {
            fContract.sentPending(foundationId, function(e, r) {
                if ( !e ) callback(r)();
                else      console.error(e);
            });
        };
    };
};

exports.todoPendingImpl = function(callback) {
    return function(addr) {
        return function() {
            fContract.todoPending(addr, function(e, r) {
                if ( !e ) callback(b2s(r))();
                else      console.error(e);
            });
        };
    };
};

exports.addPendingUnificationImpl = function(callback) {
    return function(addr) {
        return function() {
            var data = fContract.addPendingUnification.getData(addr);
            sendFoundationTx(data, 0, callback);
        };
    };
};

exports.confirmPendingUnificationImpl = function(callback) {
    return function(foundationId) {
        return function() {
            var data = fContract.confirmPendingUnification.getData(foundationId);
            sendFoundationTx(data, 0, callback);
        };
    };
};

exports.deleteAddrImpl = function(callback) {
    return function(addr) {
        return function() {
            var data = fContract.deleteAddr.getData(addr);
            sendFoundationTx(data, 0, callback);
        };
    };
};

exports.depositWeiImpl = function(callback) {
    return function(foundationId) {
        return function(weiAmountStr) {
            return function() {
                var wei = new BigNumber(weiAmountStr);
                var data = fContract.depositWei.getData();
                sendFoundationTx(data, wei, callback);
            };
        };
    };
};

exports.withdrawDepositImpl = function(callback) {
    return function(weiAmountStr) {
        return function() {
            var data = fContract.withdrawDeposit.getData(weiAmountStr);
            sendFoundationTx(data, 0, callback);
        };
    };
};

exports.getWeiToExtendImpl = function(callback) {
    return function() {
        fContract.getWeiToExtend(function(e, r) {
            if ( !e ) callback(r.valueOf())();
            else      console.error(e);
        });
    };
};

exports.extendIdOneYearImpl = function(callback) {
    return function(foundationId) {
        return function(weiToExtend) {
            return function() {
                var w = new BigNumber(weiToExtend);
                var data = fContract.extendIdOneYear.getData(foundationId);
                sendFoundationTx(data, w, callback);
            };
        };
    };
};

exports.getDepositWeiImpl = function(callback) {
    return function(foundationId) {
        return function() {
            fContract.getDepositWei(foundationId, function(e, r) {
                if ( !e ) callback(r.valueOf())();
                else      console.error(e);
            });
        };
    };
};

exports.expirationDateImpl = function(callback) {
    return function(foundationId) {
        return function() {
            fContract.getExpirationDate(foundationId, function(e, r) {
                if ( !e ) callback(parseInt(r))();
                else      console.error(e);
            });
        };
    };
};

/* ********** helpers ********** */
var sendFoundationTx = function(data, value, callback) {
    web3.eth.sendTransaction(
        {to: fAddress,
//         from: myAddress,
         data: data,
         value: value},
        function(err, result) {
            if ( !err )
                callback(goodTx(result))();
            else
                callback(errTx())();
        });
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

var goodTx = function(t) {
    return { txHash: t, error: false };
};

var errTx = function() {
    return { txHash: "", error: true };
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
        var data = fContract.addPendingUnification.getData("0xB07cd7De89Fa764301b6cC5f41eCd1497b72a475");
        web3.eth.sendTransaction({to: fAddress,
//                                  from: myAddress,
                                  data: data }, function(err, res) {
                                      console.log(res);
                                  });
    };
};
