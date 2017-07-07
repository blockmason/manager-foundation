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
