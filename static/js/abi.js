var foundationConfig =
        {
    "contract_name": "FriendInDebt",
            "abi": [{"constant":true,"inputs":[{"name":"_id1","type":"bytes32"},{"name":"_id2","type":"bytes32"}],"name":"idEq","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_addr","type":"address"},{"name":"_name","type":"bytes32"}],"name":"isUnified","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_addr1","type":"address"},{"name":"_addr2","type":"address"}],"name":"areSameId","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_name","type":"bytes32"},{"name":"amount","type":"uint256"}],"name":"withdrawDeposit","outputs":[{"name":"success","type":"bool"}],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"amount","type":"uint256"}],"name":"withdraw","outputs":[{"name":"success","type":"bool"}],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_addr","type":"address"}],"name":"deleteAddr","outputs":[],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_name","type":"bytes32"}],"name":"depositWei","outputs":[],"payable":true,"type":"function"},{"constant":true,"inputs":[{"name":"_name","type":"bytes32"}],"name":"resolveToAddresses","outputs":[{"name":"","type":"address[]"}],"payable":false,"type":"function"},{"constant":true,"inputs":[],"name":"getWeiToExtend","outputs":[{"name":"weiAmount","type":"uint256"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_addr","type":"address"}],"name":"resolveToName","outputs":[{"name":"","type":"bytes32"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_name","type":"bytes32"}],"name":"getDepositWei","outputs":[{"name":"","type":"uint256"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_name","type":"bytes32"},{"name":"index","type":"uint256"}],"name":"getAddrIndex","outputs":[{"name":"","type":"address"}],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_weiToExtend","type":"uint256"}],"name":"alterWeiToExtend","outputs":[],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"e","type":"address"},{"name":"l","type":"address[]"}],"name":"member","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_name","type":"bytes32"}],"name":"getAddrLength","outputs":[{"name":"","type":"uint256"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_addr","type":"address"}],"name":"hasName","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_name","type":"bytes32"}],"name":"confirmPendingUnification","outputs":[],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_name","type":"bytes32"}],"name":"createId","outputs":[],"payable":true,"type":"function"},{"constant":false,"inputs":[{"name":"_name","type":"bytes32"},{"name":"_addr","type":"address"}],"name":"addPendingUnification","outputs":[],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_name","type":"bytes32"}],"name":"extendIdOneYear","outputs":[],"payable":true,"type":"function"},{"inputs":[{"name":"_adminName","type":"bytes32"},{"name":"_weiToExtend","type":"uint256"}],"payable":false,"type":"constructor"}],
    "unlinked_binary": "60606040526301e1338060045534156200001857600080fd5b60405160408062002118833981016040528080519060200190919080519060200190919050505b81600081600019169055506200008b82337fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff620000b76401000000000262001c04176401000000009004565b806001819055506001600360006101000a81548160ff021916908360ff1602179055505b50506200037b565b8260066000826000191660001916815260200190815260200160002060000160009054906101000a900460ff1615620000ef57600080fd5b6200010f8484620001dc6401000000000262001abe176401000000009004565b600160066000866000191660001916815260200190815260200160002060000160006101000a81548160ff0219169083151502179055508160066000866000191660001916815260200190815260200160002060030181905550836006600086600019166000191681526020019081526020016000206005018160001916905550600060066000866000191660001916815260200190815260200160002060040181905550620001d48484620002296401000000000262001b0b176401000000009004565b5b5b50505050565b81600560008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081600019169055505b5050565b60066000836000191660001916815260200190815260200160002060010180548060010182816200025b919062000324565b916000526020600020900160005b83909190916101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff16021790555050600160066000846000191660001916815260200190815260200160002060060160008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060006101000a81548160ff0219169083151502179055505b5050565b8154818355818115116200034e578183600052602060002091820191016200034d919062000353565b5b505050565b6200037891905b80821115620003745760008160009055506001016200035a565b5090565b90565b611d8d806200038b6000396000f30060606040523615610110576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680630b42fe7a14610115578063125436b21461016157806322329ea3146101bf578063254352da1461022f5780632e1a7d4d1461027757806330077c41146102b2578063351fa818146102eb5780634fe997631461030757806350fbd3551461038457806358e31da7146103ad5780635d3bf5ba14610402578063672a3a321461043d5780636df5ae7f146104ad578063728ad4df146104d0578063a1491b8e14610561578063aeda352b1461059c578063d5cba00f146105ed578063f07f552014610614578063f20faa6814610630578063f92d7b1214610676575b600080fd5b341561012057600080fd5b61014760048080356000191690602001909190803560001916906020019091905050610692565b604051808215151515815260200191505060405180910390f35b341561016c57600080fd5b6101a5600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091908035600019169060200190919050506106a9565b604051808215151515815260200191505060405180910390f35b34156101ca57600080fd5b610215600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803573ffffffffffffffffffffffffffffffffffffffff1690602001909190505061072b565b604051808215151515815260200191505060405180910390f35b341561023a57600080fd5b61025d60048080356000191690602001909190803590602001909190505061076a565b604051808215151515815260200191505060405180910390f35b341561028257600080fd5b61029860048080359060200190919050506108cf565b604051808215151515815260200191505060405180910390f35b34156102bd57600080fd5b6102e9600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091905050610986565b005b610305600480803560001916906020019091905050610cf4565b005b341561031257600080fd5b61032c600480803560001916906020019091905050610e2c565b6040518080602001828103825283818151815260200191508051906020019060200280838360005b838110156103705780820151818401525b602081019050610354565b505050509050019250505060405180910390f35b341561038f57600080fd5b610397610f47565b6040518082815260200191505060405180910390f35b34156103b857600080fd5b6103e4600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091905050610f52565b60405180826000191660001916815260200191505060405180910390f35b341561040d57600080fd5b610427600480803560001916906020019091905050611082565b6040518082815260200191505060405180910390f35b341561044857600080fd5b61046b6004808035600019169060200190919080359060200190919050506110e5565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b34156104b857600080fd5b6104ce60048080359060200190919050506111df565b005b34156104db57600080fd5b610547600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803590602001908201803590602001908080602002602001604051908101604052809392919081815260200183836020028082843782019150505050505091905050611244565b604051808215151515815260200191505060405180910390f35b341561056c57600080fd5b6105866004808035600019169060200190919050506112c3565b6040518082815260200191505060405180910390f35b34156105a757600080fd5b6105d3600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091905050611357565b604051808215151515815260200191505060405180910390f35b34156105f857600080fd5b6106126004808035600019169060200190919050506113c6565b005b61062e6004808035600019169060200190919050506114e2565b005b341561063b57600080fd5b61067460048080356000191690602001909190803573ffffffffffffffffffffffffffffffffffffffff16906020019091905050611555565b005b61069060048080356000191690602001909190505061170b565b005b60008061069f84846117b1565b1490505b92915050565b600081426006600083600019166000191681526020019081526020016000206003015410156106d757600080fd5b610720600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205484610692565b91505b5b5092915050565b600080600061073985610f52565b915061074484610f52565b9050600061075283836117b1565b14156107615760019250610762565b5b505092915050565b60008260006107b882600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546117b1565b1415156107c457600080fd5b60066000826000191660001916815260200190815260200160002060060160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060009054906101000a900460ff16151561083857600080fd5b8260066000866000191660001916815260200190815260200160002060040154101561086357600080fd5b82600660008660001916600019168152602001908152602001600020600401600082825403925050819055503373ffffffffffffffffffffffffffffffffffffffff166108fc849081150290604051600060405180830381858888f1935050505091505b5b5092915050565b60008061091d600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546000546117b1565b14151561092957600080fd5b81600254101561093857600080fd5b816002600082825403925050819055503373ffffffffffffffffffffffffffffffffffffffff166108fc839081150290604051600060405180830381858888f1935050505090505b5b919050565b6000806000600560008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205460066000826000191660001916815260200190815260200160002060000160009054906101000a900460ff161515610a0257600080fd5b600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546000610a8d82600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546117b1565b141515610a9957600080fd5b60066000826000191660001916815260200190815260200160002060060160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060009054906101000a900460ff161515610b0d57600080fd5b600560008773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054600080600080925060009150600090505b60066000856000191660001916815260200190815260200160002060010180549050811015610c2157600060066000866000191660001916815260200190815260200160002060010182815481101515610bb457fe5b906000526020600020900160005b9054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16141515610c13578215610c0d5760019150610c21565b600192505b5b5b8080600101915050610b5e565b821515610c2d57600080fd5b811515610c3957600080fd5b600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020549850600660008a600019166000191681526020019081526020016000209750610ca2898b6119da565b96508760010187815481101515610cb557fe5b906000526020600020900160005b6101000a81549073ffffffffffffffffffffffffffffffffffffffff02191690555b5b505050505b505b5050505050565b806000610d4082600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546117b1565b141515610d4c57600080fd5b60066000826000191660001916815260200190815260200160002060060160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060009054906101000a900460ff161515610dc057600080fd5b8160066000826000191660001916815260200190815260200160002060000160009054906101000a900460ff161515610df857600080fd5b34600660008560001916600019168152602001908152602001600020600401600082825401925050819055505b5b505b5050565b610e34611cfc565b8160066000826000191660001916815260200190815260200160002060000160009054906101000a900460ff161515610e6c57600080fd5b8242600660008360001916600019168152602001908152602001600020600301541015610e9857600080fd5b600660008560001916600019168152602001908152602001600020600101805480602002602001604051908101604052809291908181526020018280548015610f3657602002820191906000526020600020905b8160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019060010190808311610eec575b505050505092505b5b505b50919050565b600060015490505b90565b6000600560008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205460066000826000191660001916815260200190815260200160002060000160009054906101000a900460ff161515610fcb57600080fd5b600560008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020544260066000836000191660001916815260200190815260200160002060030154101561103657600080fd5b600560008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205492505b5b505b50919050565b60008160066000826000191660001916815260200190815260200160002060000160009054906101000a900460ff1615156110bc57600080fd5b6006600084600019166000191681526020019081526020016000206004015491505b5b50919050565b60008260066000826000191660001916815260200190815260200160002060000160009054906101000a900460ff16151561111f57600080fd5b834260066000836000191660001916815260200190815260200160002060030154101561114b57600080fd5b600660008660001916600019168152602001908152602001600020600101805490508410151561117a57600080fd5b600660008660001916600019168152602001908152602001600020600101848154811015156111a557fe5b906000526020600020900160005b9054906101000a900473ffffffffffffffffffffffffffffffffffffffff1692505b5b505b5092915050565b600061122c600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546000546117b1565b14151561123857600080fd5b806001819055505b5b50565b600080600090505b82518110156112b7578373ffffffffffffffffffffffffffffffffffffffff16838281518110151561127a57fe5b9060200190602002015173ffffffffffffffffffffffffffffffffffffffff1614156112a957600191506112bc565b5b808060010191505061124c565b600091505b5092915050565b60008160066000826000191660001916815260200190815260200160002060000160009054906101000a900460ff1615156112fd57600080fd5b824260066000836000191660001916815260200190815260200160002060030154101561132957600080fd5b6006600085600019166000191681526020019081526020016000206001018054905092505b5b505b50919050565b6000806113a7600560008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205460006001026117b1565b1415156113b757600190506113c1565b600090506113c1565b5b919050565b80426006600083600019166000191681526020019081526020016000206003015410156113f257600080fd5b3373ffffffffffffffffffffffffffffffffffffffff1660066000846000191660001916815260200190815260200160002060020160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1614151561146a57600080fd5b6114748233611abe565b61147e8233611b0b565b600060066000846000191660001916815260200190815260200160002060020160006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055505b5b5050565b6000600154341415156114f457600080fd5b346002600082825401925050819055508160066000826000191660001916815260200190815260200160002060000160009054906101000a900460ff161561153b57600080fd5b6004544201915061154d833384611c04565b5b5b505b5050565b814260066000836000191660001916815260200190815260200160002060030154101561158157600080fd5b8260006115cd82600560003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546117b1565b1415156115d957600080fd5b60066000826000191660001916815260200190815260200160002060060160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060009054906101000a900460ff16151561164d57600080fd5b838361169882600560008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054610692565b156116a257600080fd5b8460066000886000191660001916815260200190815260200160002060020160006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055505b5b50505b505b505050565b6001543414151561171b57600080fd5b346002600082825401925050819055508060066000826000191660001916815260200190815260200160002060000160009054906101000a900460ff16151561176357600080fd5b6001543414151561177357600080fd5b346002600082825401925050819055506004544201600660008460001916600019168152602001908152602001600020600301819055505b5b505b50565b6000806000602060ff16915081602060ff1610156117d157602060ff1691505b600090505b818110156119765783816020811015156117ec57fe5b1a7f0100000000000000000000000000000000000000000000000000000000000000027effffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff1916858260208110151561183f57fe5b1a7f0100000000000000000000000000000000000000000000000000000000000000027effffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff191610156118b2577fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff92506119d2565b83816020811015156118c057fe5b1a7f0100000000000000000000000000000000000000000000000000000000000000027effffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff1916858260208110151561191357fe5b1a7f0100000000000000000000000000000000000000000000000000000000000000027effffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff1916111561196757600192506119d2565b5b5b80806001019150506117d6565b602060ff16602060ff1610156119ae577fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff92506119d2565b602060ff16602060ff1611156119c757600192506119d2565b600092506119d2565b5b5b505092915050565b60008060008090505b6006600086600019166000191681526020019081526020016000206001018054905081111515611ab1578373ffffffffffffffffffffffffffffffffffffffff1660066000876000191660001916815260200190815260200160002060010182815481101515611a4f57fe5b906000526020600020900160005b9054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff161415611aa357809150819250611ab6565b5b80806001019150506119e3565b600080fd5b505092915050565b81600560008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081600019169055505b5050565b6006600083600019166000191681526020019081526020016000206001018054806001018281611b3b9190611d10565b916000526020600020900160005b83909190916101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff16021790555050600160066000846000191660001916815260200190815260200160002060060160008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060006101000a81548160ff0219169083151502179055505b5050565b8260066000826000191660001916815260200190815260200160002060000160009054906101000a900460ff1615611c3b57600080fd5b611c458484611abe565b600160066000866000191660001916815260200190815260200160002060000160006101000a81548160ff0219169083151502179055508160066000866000191660001916815260200190815260200160002060030181905550836006600086600019166000191681526020019081526020016000206005018160001916905550600060066000866000191660001916815260200190815260200160002060040181905550611cf48484611b0b565b5b5b50505050565b602060405190810160405280600081525090565b815481835581811511611d3757818360005260206000209182019101611d369190611d3c565b5b505050565b611d5e91905b80821115611d5a576000816000905550600101611d42565b5090565b905600a165627a7a72305820b1bb91fcd7b9ef4a7a1fd67c7d871dd679a0215577d65e7d15da22041f6654d10029",
    "address": "0x7b264fd00f44dbcb566de28e5cb2a51f839ff81b",
    "network_id": 3
};
