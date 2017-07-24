var foundationConfig =
        {
    "contract_name": "Foundation",
            "abi": [{"constant":true,"inputs":[{"name":"_name","type":"bytes32"}],"name":"sentPending","outputs":[{"name":"","type":"address"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_id1","type":"bytes32"},{"name":"_id2","type":"bytes32"}],"name":"idEq","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_addr","type":"address"},{"name":"_name","type":"bytes32"}],"name":"isUnified","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"constant":false,"inputs":[],"name":"depositWei","outputs":[],"payable":true,"type":"function"},{"constant":true,"inputs":[],"name":"getWeiToCreate","outputs":[{"name":"weiAmount","type":"uint256"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_addr1","type":"address"},{"name":"_addr2","type":"address"}],"name":"areSameId","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"amount","type":"uint256"}],"name":"withdraw","outputs":[{"name":"success","type":"bool"}],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_addr","type":"address"}],"name":"deleteAddr","outputs":[],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"amount","type":"uint256"}],"name":"withdrawDeposit","outputs":[{"name":"success","type":"bool"}],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_addr","type":"address"}],"name":"addPendingUnification","outputs":[],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_name","type":"bytes32"}],"name":"resolveToAddresses","outputs":[{"name":"","type":"address[]"}],"payable":false,"type":"function"},{"constant":true,"inputs":[],"name":"getWeiToExtend","outputs":[{"name":"weiAmount","type":"uint256"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_addr","type":"address"}],"name":"resolveToName","outputs":[{"name":"","type":"bytes32"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_name","type":"bytes32"}],"name":"getDepositWei","outputs":[{"name":"","type":"uint256"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_name","type":"bytes32"},{"name":"index","type":"uint256"}],"name":"getAddrIndex","outputs":[{"name":"","type":"address"}],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_weiToExtend","type":"uint256"}],"name":"alterWeiToExtend","outputs":[],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"e","type":"address"},{"name":"l","type":"address[]"}],"name":"member","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_addr","type":"address"}],"name":"todoPending","outputs":[{"name":"","type":"bytes32"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_name","type":"bytes32"}],"name":"getExpirationDate","outputs":[{"name":"","type":"uint256"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_name","type":"bytes32"}],"name":"getAddrLength","outputs":[{"name":"","type":"uint256"}],"payable":false,"type":"function"},{"constant":true,"inputs":[{"name":"_addr","type":"address"}],"name":"hasName","outputs":[{"name":"","type":"bool"}],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_weiToCreate","type":"uint256"}],"name":"alterWeiToCreate","outputs":[],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_name","type":"bytes32"}],"name":"confirmPendingUnification","outputs":[],"payable":false,"type":"function"},{"constant":false,"inputs":[{"name":"_name","type":"bytes32"}],"name":"createId","outputs":[],"payable":true,"type":"function"},{"constant":false,"inputs":[{"name":"_name","type":"bytes32"}],"name":"extendIdOneYear","outputs":[],"payable":true,"type":"function"},{"inputs":[{"name":"_adminName","type":"bytes32"},{"name":"_weiToExtend","type":"uint256"},{"name":"_weiToCreate","type":"uint256"}],"payable":false,"type":"constructor"}],
    "unlinked_binary": "60606040526301e1338060055534156200001857600080fd5b6040516060806200232c833981016040528080519060200190919080519060200190919080519060200190919050505b82600081600019169055506200009483337fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff620000c86401000000000262001e07176401000000009004565b81600181905550806002819055506001600460006101000a81548160ff021916908360ff1602179055505b5050506200038c565b8260086000826000191660001916815260200190815260200160002060000160009054906101000a900460ff16156200010057600080fd5b620001208484620001ed6401000000000262001cc1176401000000009004565b600160086000866000191660001916815260200190815260200160002060000160006101000a81548160ff0219169083151502179055508160086000866000191660001916815260200190815260200160002060030181905550836008600086600019166000191681526020019081526020016000206005018160001916905550600060086000866000191660001916815260200190815260200160002060040181905550620001e584846200023a6401000000000262001d0e176401000000009004565b5b5b50505050565b81600760008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081600019169055505b5050565b60086000836000191660001916815260200190815260200160002060010180548060010182816200026c919062000335565b916000526020600020900160005b83909190916101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff16021790555050600160086000846000191660001916815260200190815260200160002060060160008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060006101000a81548160ff0219169083151502179055505b5050565b8154818355818115116200035f578183600052602060002091820191016200035e919062000364565b5b505050565b6200038991905b80821115620003855760008160009055506001016200036b565b5090565b90565b611f90806200039c6000396000f30060606040523615610147576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff16806305c5f41f1461014c5780630b42fe7a146101b3578063125436b2146101ff57806315488b881461025d57806320f121c31461026757806322329ea3146102905780632e1a7d4d1461030057806330077c411461033b57806333289a4614610374578063352529c8146103af5780634fe99763146103e857806350fbd3551461046557806358e31da71461048e5780635d3bf5ba146104e3578063672a3a321461051e5780636df5ae7f1461058e578063728ad4df146105b15780637b72de0b14610642578063a0f3a32114610697578063a1491b8e146106d2578063aeda352b1461070d578063b60e084d1461075e578063d5cba00f14610781578063f07f5520146107a8578063f92d7b12146107c4575b600080fd5b341561015757600080fd5b6101716004808035600019169060200190919050506107e0565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b34156101be57600080fd5b6101e560048080356000191690602001909190803560001916906020019091905050610829565b604051808215151515815260200191505060405180910390f35b341561020a57600080fd5b610243600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803560001916906020019091905050610840565b604051808215151515815260200191505060405180910390f35b6102656108c2565b005b341561027257600080fd5b61027a610930565b6040518082815260200191505060405180910390f35b341561029b57600080fd5b6102e6600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803573ffffffffffffffffffffffffffffffffffffffff1690602001909190505061093b565b604051808215151515815260200191505060405180910390f35b341561030b57600080fd5b610321600480803590602001909190505061097a565b604051808215151515815260200191505060405180910390f35b341561034657600080fd5b610372600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091905050610a31565b005b341561037f57600080fd5b6103956004808035906020019091905050610d9f565b604051808215151515815260200191505060405180910390f35b34156103ba57600080fd5b6103e6600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091905050610ec3565b005b34156103f357600080fd5b61040d600480803560001916906020019091905050611042565b6040518080602001828103825283818151815260200191508051906020019060200280838360005b838110156104515780820151818401525b602081019050610435565b505050509050019250505060405180910390f35b341561047057600080fd5b61047861115d565b6040518082815260200191505060405180910390f35b341561049957600080fd5b6104c5600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091905050611168565b60405180826000191660001916815260200191505060405180910390f35b34156104ee57600080fd5b610508600480803560001916906020019091905050611298565b6040518082815260200191505060405180910390f35b341561052957600080fd5b61054c6004808035600019169060200190919080359060200190919050506112fb565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b341561059957600080fd5b6105af60048080359060200190919050506113f5565b005b34156105bc57600080fd5b610628600480803573ffffffffffffffffffffffffffffffffffffffff169060200190919080359060200190820180359060200190808060200260200160405190810160405280939291908181526020018383602002808284378201915050505050509190505061145a565b604051808215151515815260200191505060405180910390f35b341561064d57600080fd5b610679600480803573ffffffffffffffffffffffffffffffffffffffff169060200190919050506114d9565b60405180826000191660001916815260200191505060405180910390f35b34156106a257600080fd5b6106bc600480803560001916906020019091905050611523565b6040518082815260200191505060405180910390f35b34156106dd57600080fd5b6106f760048080356000191690602001909190505061154c565b6040518082815260200191505060405180910390f35b341561071857600080fd5b610744600480803573ffffffffffffffffffffffffffffffffffffffff169060200190919050506115e0565b604051808215151515815260200191505060405180910390f35b341561076957600080fd5b61077f600480803590602001909190505061164f565b005b341561078c57600080fd5b6107a66004808035600019169060200190919050506116b4565b005b6107c2600480803560001916906020019091905050611843565b005b6107de60048080356000191690602001909190505061190e565b005b600060086000836000191660001916815260200190815260200160002060020160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1690505b919050565b60008061083684846119b4565b1490505b92915050565b6000814260086000836000191660001916815260200190815260200160002060030154101561086e57600080fd5b6108b7600760008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205484610829565b91505b5b5092915050565b3460086000600760003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205460001916600019168152602001908152602001600020600401600082825401925050819055505b565b600060025490505b90565b600080600061094985611168565b915061095484611168565b9050600061096283836119b4565b14156109715760019250610972565b5b505092915050565b6000806109c8600760003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546000546119b4565b1415156109d457600080fd5b8160035410156109e357600080fd5b816003600082825403925050819055503373ffffffffffffffffffffffffffffffffffffffff166108fc839081150290604051600060405180830381858888f1935050505090505b5b919050565b6000806000600760008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205460086000826000191660001916815260200190815260200160002060000160009054906101000a900460ff161515610aad57600080fd5b600760008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546000610b3882600760003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546119b4565b141515610b4457600080fd5b60086000826000191660001916815260200190815260200160002060060160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060009054906101000a900460ff161515610bb857600080fd5b600760008773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054600080600080925060009150600090505b60086000856000191660001916815260200190815260200160002060010180549050811015610ccc57600060086000866000191660001916815260200190815260200160002060010182815481101515610c5f57fe5b906000526020600020900160005b9054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16141515610cbe578215610cb85760019150610ccc565b600192505b5b5b8080600101915050610c09565b821515610cd857600080fd5b811515610ce457600080fd5b600760003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020549850600860008a600019166000191681526020019081526020016000209750610d4d898b611bdd565b96508760010187815481101515610d6057fe5b906000526020600020900160005b6101000a81549073ffffffffffffffffffffffffffffffffffffffff02191690555b5b505050505b505b5050505050565b60008082111515610daf57600080fd5b8160086000600760003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054600019166000191681526020019081526020016000206004015410151515610e1b57600080fd5b8160086000600760003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205460001916600019168152602001908152602001600020600401600082825403925050819055503373ffffffffffffffffffffffffffffffffffffffff166108fc839081150290604051600060405180830381858888f1935050505090505b919050565b806000801916600760008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205460001916141515610f1957600080fd5b8160086000600760003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546000191660001916815260200190815260200160002060020160006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550600760003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054600660008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081600019169055505b5b5050565b61104a611eff565b8160086000826000191660001916815260200190815260200160002060000160009054906101000a900460ff16151561108257600080fd5b82426008600083600019166000191681526020019081526020016000206003015410156110ae57600080fd5b60086000856000191660001916815260200190815260200160002060010180548060200260200160405190810160405280929190818152602001828054801561114c57602002820191906000526020600020905b8160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019060010190808311611102575b505050505092505b5b505b50919050565b600060015490505b90565b6000600760008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205460086000826000191660001916815260200190815260200160002060000160009054906101000a900460ff1615156111e157600080fd5b600760008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020544260086000836000191660001916815260200190815260200160002060030154101561124c57600080fd5b600760008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205492505b5b505b50919050565b60008160086000826000191660001916815260200190815260200160002060000160009054906101000a900460ff1615156112d257600080fd5b6008600084600019166000191681526020019081526020016000206004015491505b5b50919050565b60008260086000826000191660001916815260200190815260200160002060000160009054906101000a900460ff16151561133557600080fd5b834260086000836000191660001916815260200190815260200160002060030154101561136157600080fd5b600860008660001916600019168152602001908152602001600020600101805490508410151561139057600080fd5b600860008660001916600019168152602001908152602001600020600101848154811015156113bb57fe5b906000526020600020900160005b9054906101000a900473ffffffffffffffffffffffffffffffffffffffff1692505b5b505b5092915050565b6000611442600760003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546000546119b4565b14151561144e57600080fd5b806001819055505b5b50565b600080600090505b82518110156114cd578373ffffffffffffffffffffffffffffffffffffffff16838281518110151561149057fe5b9060200190602002015173ffffffffffffffffffffffffffffffffffffffff1614156114bf57600191506114d2565b5b8080600101915050611462565b600091505b5092915050565b6000600660008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205490505b919050565b60006008600083600019166000191681526020019081526020016000206003015490505b919050565b60008160086000826000191660001916815260200190815260200160002060000160009054906101000a900460ff16151561158657600080fd5b82426008600083600019166000191681526020019081526020016000206003015410156115b257600080fd5b6008600085600019166000191681526020019081526020016000206001018054905092505b5b505b50919050565b600080611630600760008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205460006001026119b4565b141515611640576001905061164a565b6000905061164a565b5b919050565b600061169c600760003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546000546119b4565b1415156116a857600080fd5b806002819055505b5b50565b336000801916600760008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020546000191614151561170a57600080fd5b3373ffffffffffffffffffffffffffffffffffffffff1660086000846000191660001916815260200190815260200160002060020160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1614151561178257600080fd5b61178c8233611cc1565b6117968233611d0e565b600060086000846000191660001916815260200190815260200160002060020160006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055506000600660003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081600019169055505b5b5050565b60006002543414151561185557600080fd5b34600360008282540192505081905550336000801916600760008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054600019161415156118bb57600080fd5b8260086000826000191660001916815260200190815260200160002060000160009054906101000a900460ff16156118f257600080fd5b60055442019250611904843385611e07565b5b5b505b505b5050565b6001543414151561191e57600080fd5b346003600082825401925050819055508060086000826000191660001916815260200190815260200160002060000160009054906101000a900460ff16151561196657600080fd5b6001543414151561197657600080fd5b346003600082825401925050819055506005544201600860008460001916600019168152602001908152602001600020600301819055505b5b505b50565b6000806000602060ff16915081602060ff1610156119d457602060ff1691505b600090505b81811015611b795783816020811015156119ef57fe5b1a7f0100000000000000000000000000000000000000000000000000000000000000027effffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff19168582602081101515611a4257fe5b1a7f0100000000000000000000000000000000000000000000000000000000000000027effffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff19161015611ab5577fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff9250611bd5565b8381602081101515611ac357fe5b1a7f0100000000000000000000000000000000000000000000000000000000000000027effffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff19168582602081101515611b1657fe5b1a7f0100000000000000000000000000000000000000000000000000000000000000027effffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff19161115611b6a5760019250611bd5565b5b5b80806001019150506119d9565b602060ff16602060ff161015611bb1577fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff9250611bd5565b602060ff16602060ff161115611bca5760019250611bd5565b60009250611bd5565b5b5b505092915050565b60008060008090505b6008600086600019166000191681526020019081526020016000206001018054905081111515611cb4578373ffffffffffffffffffffffffffffffffffffffff1660086000876000191660001916815260200190815260200160002060010182815481101515611c5257fe5b906000526020600020900160005b9054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff161415611ca657809150819250611cb9565b5b8080600101915050611be6565b600080fd5b505092915050565b81600760008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081600019169055505b5050565b6008600083600019166000191681526020019081526020016000206001018054806001018281611d3e9190611f13565b916000526020600020900160005b83909190916101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff16021790555050600160086000846000191660001916815260200190815260200160002060060160008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060006101000a81548160ff0219169083151502179055505b5050565b8260086000826000191660001916815260200190815260200160002060000160009054906101000a900460ff1615611e3e57600080fd5b611e488484611cc1565b600160086000866000191660001916815260200190815260200160002060000160006101000a81548160ff0219169083151502179055508160086000866000191660001916815260200190815260200160002060030181905550836008600086600019166000191681526020019081526020016000206005018160001916905550600060086000866000191660001916815260200190815260200160002060040181905550611ef78484611d0e565b5b5b50505050565b602060405190810160405280600081525090565b815481835581811511611f3a57818360005260206000209182019101611f399190611f3f565b5b505050565b611f6191905b80821115611f5d576000816000905550600101611f45565b5090565b905600a165627a7a72305820f230e85f96acd644bb9af9b473cf60fd134a1e658cf0f90b01c111ab545fd20d0029",
    "address": "0x694a92520101d8f78a7aba2578380628565e3621",
    "network_id": 3
};
