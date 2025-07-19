# 💉 Immunichain - Vaccination Record NFTs

## 🌟 Overview

Immunichain is a blockchain-based vaccination record system built on the Stacks blockchain using Clarity smart contracts. It creates immutable, verifiable vaccination records as NFTs, providing a secure and transparent way to manage vaccination history.

## ✨ Features

- 🏥 **Authorized Issuers**: Only authorized healthcare providers can mint vaccination records
- 📋 **Comprehensive Records**: Store detailed vaccination information including vaccine name, batch, date, location, and dose number
- 🔍 **Verification System**: Built-in verification mechanism for vaccination records
- 📊 **Patient History**: Track complete vaccination history by patient ID
- 💰 **Marketplace**: Built-in marketplace for transferring vaccination records
- 🔒 **Immutable**: Records are permanently stored on the blockchain

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing

### Installation

```bash
clarinet new immunichain-project
cd immunichain-project
```

Copy the contract code into `contracts/Immunichain.clar`

### Testing

```bash
clarinet console
```

## 📖 Usage

### For Contract Owner

#### Authorize Healthcare Provider
```clarity
(contract-call? .Immunichain authorize-issuer 'SP1HEALTHCARE-PROVIDER-ADDRESS)
```

#### Set Commission Rate
```clarity
(contract-call? .Immunichain set-commission u250)
```

### For Authorized Issuers

#### Mint Vaccination Record
```clarity
(contract-call? .Immunichain mint-vaccination-record 
  'SP1PATIENT-ADDRESS
  "PATIENT-001"
  "COVID-19 mRNA Vaccine"
  "BATCH-12345"
  u1640995200
  "City Hospital"
  u1
  (some u1672531200))
```

### For Patients/Users

#### Check Vaccination History
```clarity
(contract-call? .Immunichain get-patient-records "PATIENT-001")
```

#### List Record for Sale
```clarity
(contract-call? .Immunichain list-in-ustx u1 u1000000 u1672531200)
```

#### Transfer Record
```clarity
(contract-call? .Immunichain transfer u1 tx-sender 'SP1RECIPIENT-ADDRESS)
```

## 🔧 Contract Functions

### Public Functions

| Function | Description |
|----------|-------------|
| `authorize-issuer` | Authorize healthcare provider to issue records |
| `mint-vaccination-record` | Create new vaccination record NFT |
| `transfer` | Transfer record to another address |
| `list-in-ustx` | List record for sale in marketplace |
| `buy-in-ustx` | Purchase listed record |
| `verify-record` | Mark record as verified |

### Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-vaccination-record` | Get detailed record information |
| `get-patient-records` | Get all records for a patient |
| `get-vaccination-history` | Get complete vaccination history |
| `is-authorized-issuer` | Check if address is authorized issuer |

## 🏗️ Data Structure

Each vaccination record contains:
- Patient ID
- Vaccine name and batch number
- Vaccination date and location
- Dose number and next dose due date
- Issuer information
- Verification status

## 🛡️ Security Features

- Only authorized issuers can mint records
- Immutable record storage
- Owner-only transfer restrictions
- Built-in verification system
- Commission-based marketplace

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

This project is licensed under the MIT License.

## 🆘 Support

For support and questions, please open an issue in the repository.


