var friendInDebtConfig = {
    "contract_name": "FriendInDebt",
    "abi": [
    {
      "constant": false,
      "inputs": [
        {
          "name": "creditor",
          "type": "address"
        }
      ],
      "name": "cancelPending",
      "outputs": [],
      "payable": false,
      "type": "function"
    },
    {
      "constant": true,
      "inputs": [
        {
          "name": "debtor",
          "type": "address"
        },
        {
          "name": "creditor",
          "type": "address"
        }
      ],
      "name": "getPendingAmount",
      "outputs": [
        {
          "name": "",
          "type": "int256"
        }
      ],
      "payable": false,
      "type": "function"
    },
    {
      "constant": false,
      "inputs": [
        {
          "name": "debtor",
          "type": "address"
        },
        {
          "name": "amount",
          "type": "int256"
        }
      ],
      "name": "newPending",
      "outputs": [],
      "payable": false,
      "type": "function"
    },
    {
      "constant": false,
      "inputs": [
        {
          "name": "person",
          "type": "address"
        }
      ],
      "name": "getFriends",
      "outputs": [
        {
          "name": "",
          "type": "address[]"
        }
      ],
      "payable": false,
      "type": "function"
    },
    {
      "constant": false,
      "inputs": [
        {
          "name": "friend",
          "type": "address"
        }
      ],
      "name": "createFriendship",
      "outputs": [],
      "payable": false,
      "type": "function"
    },
    {
      "constant": false,
      "inputs": [
        {
          "name": "friend1",
          "type": "address"
        },
        {
          "name": "friend2",
          "type": "address"
        }
      ],
      "name": "checkMutual",
      "outputs": [
        {
          "name": "",
          "type": "bool"
        }
      ],
      "payable": false,
      "type": "function"
    },
    {
      "constant": true,
      "inputs": [
        {
          "name": "debtor",
          "type": "address"
        },
        {
          "name": "creditor",
          "type": "address"
        }
      ],
      "name": "getBalance",
      "outputs": [
        {
          "name": "",
          "type": "int256"
        }
      ],
      "payable": false,
      "type": "function"
    },
    {
      "constant": false,
      "inputs": [
        {
          "name": "creditor",
          "type": "address"
        },
        {
          "name": "amount",
          "type": "int256"
        }
      ],
      "name": "confirmPending",
      "outputs": [],
      "payable": false,
      "type": "function"
    },
    {
      "inputs": [],
      "payable": false,
      "type": "constructor"
    }
  ],
    "unlinked_binary": "0x6060604052341561000c57fe5b5b5b5b610d928061001e6000396000f3006060604052361561008c576000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680630cbfb5b11461008e57806337079d63146100c45780635a5653cd1461012d5780635cbb7caa1461016c57806365bfbad814610205578063b497804f1461023b578063d4fac45d146102a8578063da85de4d14610311575bfe5b341561009657fe5b6100c2600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091905050610350565b005b34156100cc57fe5b610117600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803573ffffffffffffffffffffffffffffffffffffffff169060200190919050506103ec565b6040518082815260200191505060405180910390f35b341561013557fe5b61016a600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803590602001909190505061048a565b005b341561017457fe5b6101a0600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190505061053e565b60405180806020018281038252838181518152602001915080519060200190602002808383600083146101f2575b8051825260208311156101f2576020820191506020810190506020830392506101ce565b5050509050019250505060405180910390f35b341561020d57fe5b610239600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091905050610612565b005b341561024357fe5b61028e600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803573ffffffffffffffffffffffffffffffffffffffff169060200190919050506108e5565b604051808215151515815260200191505060405180910390f35b34156102b057fe5b6102fb600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190803573ffffffffffffffffffffffffffffffffffffffff16906020019091905050610a24565b6040518082815260200191505060405180910390f35b341561031957fe5b61034e600480803573ffffffffffffffffffffffffffffffffffffffff16906020019091908035906020019091905050610b40565b005b61035a33826108e5565b15156103665760006000fd5b6000600360003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055505b50565b60006103f883836108e5565b15156104045760006000fd5b600360008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000205490505b92915050565b61049433836108e5565b15156104a05760006000fd5b60008114156104af5760006000fd5b80600360008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600082825401925050819055505b5050565b610546610d01565b600060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002080548060200260200160405190810160405280929190818152602001828054801561060557602002820191906000526020600020905b8160009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190600101908083116105bb575b505050505090505b919050565b600160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060009054906101000a900460ff16156106a75760006000fd5b6001600160003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060006101000a81548160ff0219169083151502179055506000600260003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506000600360003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550600060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002080548060010182816108919190610d15565b916000526020600020900160005b83909190916101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550505b50565b6000600160008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060009054906101000a900460ff1615156109805760009050610a1e565b600160008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060009054906101000a900460ff161515610a195760009050610a1e565b600190505b92915050565b6000610a3083836108e5565b1515610a3c5760006000fd5b600260008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054600260008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020540390505b92915050565b610b4a33836108e5565b1515610b565760006000fd5b6000811415610b655760006000fd5b80600360003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054141515610bf05760006000fd5b6000600360003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190555080600260003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020600082825401925050819055505b5050565b602060405190810160405280600081525090565b815481835581811511610d3c57818360005260206000209182019101610d3b9190610d41565b5b505050565b610d6391905b80821115610d5f576000816000905550600101610d47565b5090565b905600a165627a7a7230582054a8bc0ac5d7c92416ff4ff222b113efa649b6b926f385994fe03f588cfb83ce0029",
    "address": "0x63d633de3fa2fbd6a7ada6f7af6317d897e29269",
    "network_id": 1
};

var friendInDebtNSConfig = {
    "contract_name": "FriendInDebtNS",
    "abi": [
    {
      "constant": false,
      "inputs": [
        {
          "name": "_name",
          "type": "string"
        }
      ],
      "name": "setName",
      "outputs": [],
      "payable": false,
      "type": "function"
    },
    {
      "constant": true,
      "inputs": [
        {
          "name": "_user",
          "type": "address"
        }
      ],
      "name": "getName",
      "outputs": [
        {
          "name": "",
          "type": "string"
        }
      ],
      "payable": false,
      "type": "function"
    },
    {
      "inputs": [],
      "payable":false,
      "type":"constructor"
    }
  ],
    "unlinked_binary": "0x60606040526000357c0100000000000000000000000000000000000000000000000000000000900463ffffffff1680635fd4b08a14610046578063c47f002714610103575bfe5b341561004e57fe5b61007a600480803573ffffffffffffffffffffffffffffffffffffffff1690602001909190505061015d565b60405180806020018281038252838181518152602001915080519060200190808383600083146100c9575b8051825260208311156100c9576020820191506020810190506020830392506100a5565b505050905090810190601f1680156100f55780820380516001836020036101000a031916815260200191505b509250505060405180910390f35b341561010b57fe5b61015b600480803590602001908201803590602001908080601f01602080910402602001604051908101604052809392919081815260200183838082843782019150505050505091905050610245565b005b61016561029d565b600060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208054600181600116156101000203166002900480601f0160208091040260200160405190810160405280929190818152602001828054600181600116156101000203166002900480156102385780601f1061020d57610100808354040283529160200191610238565b820191906000526020600020905b81548152906001019060200180831161021b57829003601f168201915b505050505090505b919050565b80600060003373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002090805190602001906102989291906102b1565b505b50565b602060405190810160405280600081525090565b828054600181600116156101000203166002900490600052602060002090601f016020900481019282601f106102f257805160ff1916838001178555610320565b82800160010185558215610320579182015b8281111561031f578251825591602001919060010190610304565b5b50905061032d9190610331565b5090565b61035391905b8082111561034f576000816000905550600101610337565b5090565b905600a165627a7a72305820f58e620b3d4ce158bfac3b8b4bf6ba92c8f0148a81a316753a4fb066fd7ca4cf0029",
    "address": "0xd58f4d92488ee02fe0de5e2916a9709f2ff83b05",
    "network_id": 1
};
