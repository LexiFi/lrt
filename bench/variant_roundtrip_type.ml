(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.    *)
(******************************************************************************)

open Lrt

type currency =
  | USD  (** US Dollar*)
  | JPY  (** Japanese Yen*)
  | EUR  (** Euro*)
  | GBP  (** Pound Sterling*)
  | CHF  (** Swiss Franc*)
  | HKD  (** Hong Kong Dollar*)
  | CAD  (** Canadian Dollar*)
  | AUD  (** Australian Dollar*)
  | DKK  (** Danish Krone*)
  | NOK  (** Norvegian Krone*)
  | SEK  (** Swedish Krona*)
  | CZK  (** Czech Koruna*)
  | IEP  (** Irish Pound*)
  | MYR  (** Malayasian Ringgit*)
  | NZD  (** New Zealand Dollar*)
  | SGD  (** Singapore Dollar*)
  | THB  (** Thai Baht*)
  | ZAR  (** South African Rand*)
  | FIM  (** Markka*)
  | PTE  (** Portuguese Escudo*)
  | IDR  (** Indonesian Rupiah*)
  | TWD  (** New Taiwan Dollar*)
  | EEK  (** Estonian Kroon*)
  | HUF  (** Hungarian Forint*)
  | ARS  (** Argentine peso*)
  | BRL  (** Brazilian Real*)
  | CLP  (** Chiliean Peso*)
  | FRF
  | DEM
  | ITL
  | ESP
  | BEF
  | ATS
  | NLG
  | LUF
  | GRD  (** Euroland*)
  | ILS  (** Israeli Shekel*)
  | KRW  (** Korean Won*)
  | LBP  (** Libanese Pound*)
  | MXP  (** Mexican Peso*)
  | PHP  (** Philippine peso*)
  | PLZ  (** Polish Zloty*)
  | RUB  (** Russian Ruble*)
  | SAR  (** Saudi Arabian Riyal *)
  | SKK  (** Slovak Koruna*)
  | TRL  (** Turkish Lira*)
  | CNY  (** Chinese Yuan Renminbi*)
  | INR  (** Indian Rupee*)
  | MXN
  | TRY
  | PLN
  | IRR
  | AED
  | VEF
  | COP
  | EGP
  | NGN
  | PKR
  | RON
  | DZD
  | PEN
  | KZT
  | UAH
  | KWD
  | QAR
  | BDT
  | VND
  | MAD
  | XAU
  | XAG
  | XPT
  | XPD
  | GBX  (** Pence Sterling  (100 GBX = 1 GBP) *)
  | LKR  (** Sri Lankan rupee *)
  | CNH  (** Huan Offshore *)
  | AFN
  | ALL
  | AOA
  | AZN
  | BAM
  | BGN
  | BHD
  | BOB
  | BWP
  | BYR
  | CDF
  | CRC
  | CUC
  | CUP
  | DOP
  | ETB
  | GHS
  | GTQ
  | HNL
  | HRK
  | IQD
  | JOD
  | KES
  | KHR
  | MMK
  | MZN
  | NPR
  | OMR
  | PAB
  | PYG
  | RSD
  | SDG
  | SVC
  | SYP
  | TMT
  | TND
  | TZS
  | UGX
  | UYU
  | UZS
  | YER
  | XCD  (** East Caribbean Dollar *)
  | AMD  (** Armenia Dram *)
  | ANG  (** Netherlands Antilles Guilder *)
  | AWG  (** Aruba Guilder *)
  | BBD  (** Barbados Dollar *)
  | BIF  (** Burundi Franc *)
  | BMD  (** Bermuda Dollar *)
  | BND  (** Brunei Darussalam Dollar *)
  | BSD  (** Bahamas Dollar *)
  | BZD  (** Belize Dollar *)
  | CVE  (** Cape Verde Escudo *)
  | DJF  (** Djibouti Franc *)
  | ERN  (** Eritrea Nakfa *)
  | FJD  (** Fiji Dollar *)
  | FKP  (** Falkland Islands (Malvinas) Pound *)
  | GEL  (** Georgia Lari *)
  | GIP  (** Gibraltar Pound *)
  | GMD  (** Gambia Dalasi *)
  | GNF  (** Guinea Franc *)
  | GYD  (** Guyana Dollar *)
  | ISK  (** Iceland Krona *)
  | JMD  (** Jamaica Dollar *)
  | KGS  (** Kyrgyzstan Som *)
  | KMF  (** Comoros Franc *)
  | KPW  (** Korea (North) Won *)
  | KYD  (** Cayman Islands Dollar *)
  | LAK  (** Laos Kip *)
  | LRD  (** Liberia Dollar *)
  | LYD  (** Libya Dinar *)
  | MDL  (** Moldova Leu *)
  | MGA  (** Madagascar Ariary *)
  | MKD  (** Macedonia Denar *)
  | MNT  (** Mongolia Tughrik *)
  | MOP  (** Macau Pataca *)
  | MRO  (** Mauritania Ouguiya *)
  | MUR  (** Mauritius Rupee *)
  | MVR  (** Maldives (Maldive Islands) Rufiyaa *)
  | MWK  (** Malawi Kwacha *)
  | NIO  (** Nicaragua Cordoba *)
  | PGK  (** Papua New Guinea Kina *)
  | RWF  (** Rwanda Franc *)
  | SBD  (** Solomon Islands Dollar *)
  | SCR  (** Seychelles Rupee *)
  | SHP  (** Saint Helena Pound *)
  | SLL  (** Sierra Leone Leone *)
  | SOS  (** Somalia Shilling *)
  | SRD  (** Suriname Dollar *)
  | SSP  (** South Sudanese pound *)
  | STD  (** São Tomé and Príncipe Dobra *)
  | SZL  (** Swaziland Lilangeni *)
  | TJS  (** Tajikistan Somoni *)
  | TOP  (** Tonga Pa'anga *)
  | TTD  (** Trinidad and Tobago Dollar *)
  | VUV  (** Vanuatu Vatu *)
  | WST  (** Samoa Tala *)
  | XAF  (** Communauté Financière Africaine (BEAC) CFA Franc BEAC *)
  | XOF  (** Communauté Financière Africaine (BCEAO) Franc *)
  | XPF  (** Comptoirs Français du Pacifique (CFP) Franc *)
  | ZMW  (** Zambia Kwacha *)
  | ZWL  (** Zimbabwean dollar *)
  | CLF  (** Chile Unidad de Fomento *)
  | LTL  (** Lithuanian Litas (now Euroland) *)
  | Asset of string  (** Another asset, considered as a currency. *)
[@@deriving t]

type rec1 =
  { rec1_f1: string
  ; rec1_f2: int
  ; rec1_f3: int * string
  ; rec1_f4: bool
  ; rec1_f5: float list }

and rec2 = {rec2_f1: float; rec2_f2: float; rec2_i1: int}

and variant =
  | R1 of rec1
  | R2 of rec2
  | V1 of bool option array
  | V2 of currency list
  | E1

and t = variant * variant [@@deriving t]
