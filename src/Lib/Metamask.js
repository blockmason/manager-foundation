//requires web3 js and truffle-contract js
"use strict";

exports.checkStatusImpl = function(dummyVal) {
    return function() {
        //check whether metamask is logged in
        if (web3.eth.accounts[0] == undefined) {
            return false;
        }
        else {
            return true;
        }
    };
};

exports.currentUserImpl = function(dummyVal) {
    return function() {
        return web3.eth.accounts[0];
    };
};

exports.printTransactionImpl = function(unit) {
    return function() {
        web3.eth.getTransaction("0x7a406952d4bcc5f600a397f4101fe64b4a6116a08a4a8f7d79ded9ecbbde51c3", function(error, result) {
            console.log(result);
        });
    };
};
