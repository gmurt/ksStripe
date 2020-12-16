{ *******************************************************************************
*                                                                              *
*  ksStripe - Stripe Interface for Delphi                                      *
*                                                                              *
*  https://github.com/gmurt/ksStripe                                           *
*                                                                              *
*  Copyright 2020 Graham Murt                                                  *
*                                                                              *
*  email: graham@kernow-software.co.uk                                         *
*                                                                              *
*  Licensed under the Apache License, Version 2.0 (the "License");             *
*  you may not use this file except in compliance with the License.            *
*  You may obtain a copy of the License at                                     *
*                                                                              *
*    http://www.apache.org/licenses/LICENSE-2.0                                *
*                                                                              *
*  Unless required by applicable law or agreed to in writing, software         *
*  distributed under the License is distributed on an "AS IS" BASIS,           *
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.    *
*  See the License for the specific language governing permissions and         *
*  limitations under the License.                                              *
*                                                                              *
*******************************************************************************}

unit ksStripe;

interface

uses Classes, Generics.Collections

{$DEFINE JSONDATAOBJECTS}

  {$IFDEF JSONDATAOBJECTS}
  , JsonDataObjects
  {$ELSE}
  , Json
  {$ENDIF}
;

{$IF CompilerVersion >= 29}
  {$DEFINE USE_NET_HTTP}
{$ENDIF}

type
  TStripeCurrency = (scUnknown, scGbp, scUsd);

//------------------------------------------------------------------------------



  IStripeBaseObject = interface
  ['{AC396FFE-A89C-4811-8DDD-5A3A69546155}']
    function GetID: string;
    function GetObject: string;
    procedure SetID(const Value: string);
    procedure Clear;
    procedure LoadFromJson(AJson: TJsonObject);
    property ID: string read GetID write SetID;
    property Obj: string read GetObject;
  end;

//------------------------------------------------------------------------------

  IStripeBaseObjectList<T> = interface
  ['{3FD36F72-3FF3-4377-AE0E-120A19C63354}']
    function GetCount: integer;
    function GetItem(index: integer): T;
    function GetListID: string;
    procedure Clear;
    procedure LoadFromJson(AJson: TJSONObject);
    property Count: integer read GetCount;
    property Item[index: integer]: T read GetItem; default;
  end;

//------------------------------------------------------------------------------

  IStripeCard = interface(IStripeBaseObject)
  ['{76652D56-42CE-4C2F-B0B2-1E6485D501AD}']
    function GetBrand: string;
    function GetLast4: string;
    function GetExpMonth: integer;
    function GetExpYear: integer;
    property Last4: string read GetLast4;
    property Brand: string read GetBrand;
    property ExpMonth: integer read GetExpMonth;
    property ExpYear: integer read GetExpYear;
  end;

//------------------------------------------------------------------------------

  IStripeCharge = interface(IStripeBaseObject)
  ['{197B9D1A-B4F1-4220-AFDC-22DE5031F1B4}']
    function GetCreated: TDatetime;
    function GetLiveMode: Boolean;
    function GetPaid: Boolean;
    function GetStatus: string;
    function GetAmountPence: integer;
    function GetCurrency: TStripeCurrency;
    function GetRefunded: Boolean;
    function GetCard: IStripeCard;
    function GetCustomer: string;
    function GetDesc: string;
    property Created: TDateTime read GetCreated;
    property LiveMode: Boolean read GetLiveMode;
    property Paid: Boolean read GetPaid;
    property Status: string read GetStatus;
    property AmountPence: integer read GetAmountPence;
    property Currency: TStripeCurrency read GetCurrency;
    property Refunded: Boolean read GetRefunded;
    property Customer: string read GetCustomer;
    property Card: IStripeCard read GetCard;
    property Desc: string read GetDesc;
  end;

//------------------------------------------------------------------------------

  IStripePlan = interface(IStripeBaseObject)
  ['{E37D8D42-0FDE-4108-BD58-56603955FDCC}']
    function GetAmountPence: integer;
    function GetCreated: TDateTime;
    function GetCurrency: TStripeCurrency;
    function GetInterval: string;
    function GetIntervalCount: integer;
    function GetName: string;
    function GetStatementDescriptor: string;
    function GetTrialPeriodDays: integer;
    property Interval: string read GetInterval;
    property Name: string read GetName;
    property Created: TDateTime read GetCreated;
    property AmountPence: integer read GetAmountPence;
    property Currency: TStripeCurrency read GetCurrency;
    property IntervalCount: integer read GetIntervalCount;
    property TrialPeriodDays: integer read GetTrialPeriodDays;
    property StatementDescriptor: string read GetStatementDescriptor;
  end;

//------------------------------------------------------------------------------

  IStripeSubscription = interface(IStripeBaseObject)
  ['{3F2BE016-7483-4020-BEB6-F0A3B55E9753}']
    function GetCancelledAt: TDateTime;
    function GetCurrentPeriodEnd: TDateTime;
    function GetCurrentPeriodStart: TDateTime;
    function GetCustomer: string;
    function GetEndedAt: TDateTime;
    function GetPlan: IStripePlan;
    function GetQuantity: integer;
    function GetStart: TDateTime;
    function GetStatus: string;
    function GetTaxPercent: single;
    function GetTrialEnd: TDateTime;
    function GetTrialStart: TDateTime;
    property Plan: IStripePlan read GetPlan;
    property Start: TDateTime read GetStart;
    property Status: string read GetStatus;
    property Customer: string read GetCustomer;
    property CurrentPeriodStart: TDateTime read GetCurrentPeriodStart;
    property CurrentPeriodEnd: TDateTime read GetCurrentPeriodEnd;
    property EndedAt: TDateTime read GetEndedAt;
    property TrialStart: TDateTime read GetTrialStart;
    property TrialEnd: TDateTime read GetTrialEnd;
    property CancelledAt: TDateTime read GetCancelledAt;
    property Quantity: integer read GetQuantity;
    property TaxPercent: single read GetTaxPercent;
  end;

//------------------------------------------------------------------------------

  IStripeCustomer = interface(IStripeBaseObject)
  ['{CFA07B51-F63C-4972-ACAB-FA51D6DF5779}']
    function GetEmail: string;
    function GetName: string;

    function GetDescription: string;
    procedure SetName(const Value: string);
    procedure SetEmail(const Value: string);
    procedure Assign(ACustomer: IStripeCustomer);
    procedure SetDescription(const Value: string);
    property Name: string read GetName write SetName;
    property Email: string read GetEmail write SetEmail;
    property Description: string read GetDescription write SetDescription;
  end;

//------------------------------------------------------------------------------

  IStripeCustomerList = interface(IStripeBaseObjectList<IStripeCustomer>)
  ['{A84D8E11-C142-4E4C-9698-A6DFBCE14742}']
  end;

//------------------------------------------------------------------------------

  IStripe = interface
  ['{A00E2188-0DDB-469F-9C4A-0900DEEFD27B}']
    function GetLastError: string;
    function GetLastJsonResult: string;
    function CreateToken(ACardNum: string; AExpMonth, AExpYear: integer; ACvc: string): string;
    function CreateCharge(AToken, ADescription: string;
                          AAmountPence: integer;
                          AMetaData: TStrings;
                          var AError: string;
                          const ACurrency: TStripeCurrency = scGbp): IStripeCharge;
    function CreateCard(ACustID, ACardToken: string; var AError: string): IStripeCard; overload;
    function CreateCard(ACustID, ACardNum: string; AExpMonth, AExpYear: integer; ACvc: string; var AError: string): IStripeCard; overload;
    function DeleteCard(ACustID, ACardID: string): Boolean;
    function UpdateDefaultSource(ACustID, ACardID: string): Boolean;
    function CreateChargeForCustomer(ACustID, ADescription: string; AAmountPence: integer; const ACurrency: TStripeCurrency = scGbp): IStripeCharge;
    function RefundCharge(AChargeID: string): string;
    function GetCustomer(ACustID: string): IStripeCustomer;
    function CustomerExists(ACustID: string; ACustomer: IStripeCustomer): Boolean;
    function GetAccount: string;

    function GetCapabilities: string;
    function GetPersons: string;
    function GetCustomers(const ACreatedAfter: TDateTime = -1;
                          const ACreatedBefore: TDateTime = -1;
                          const ALimit: Integer = 10): IStripeCustomerList;
    function CreateCustomer(AEmail, ADescription: string): IStripeCustomer;
    function GetBalance: Extended;
    procedure UpdateCustomerValue(ACustID, AField, AValue: string);
    property LastError: string read GetLastError;
    property LastJsonResult: string read GetLastJsonResult;

  end;

//------------------------------------------------------------------------------

  function CreateStripe(ASecretKey: string): IStripe;
  function CreateStripeCustomer: IStripeCustomer;

implementation

uses
  SysUtils, DateUtils,
  {$IFDEF USE_NET_HTTP}
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent;
  {$ELSE}
  IdHttp, IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdAuthentication;
  {$ENDIF}

const
  C_ACCOUNT = 'account';
  C_CAPABILITIES = 'capabilities';
  C_BALANCE = 'balance';
  C_CARD = 'card';
  C_CARDS = 'cards';
  C_CHARGE = 'charge';
  C_CHARGES = 'charges';
  C_CUSTOMER = 'customer';
  C_CUSTOMERS = 'customers';
  C_TOKEN  = 'token';
  C_TOKENS = 'tokens';
  C_PERSONS = 'persons';
  C_PLAN = 'plan';
  C_PLANS = 'plans';
  C_SUBSCRIPTION = 'subscription';
  C_SUBSCRIPTIONS = 'subscriptions';

type
//------------------------------------------------------------------------------

  TStripeBaseObject = class(TInterfacedObject, IStripeBaseObject)
  strict private
    FJson: TJSONObject;
    FId: string;
  private
    procedure SetID(const Value: string);
  protected
    function GetID: string;
    function GetObject: string; virtual; abstract;
    procedure LoadFromJson(AJson: TJsonObject); virtual;
  public
    constructor Create; virtual;
    procedure Clear; virtual;
    property ID: string read GetID write SetID;
  end;

//------------------------------------------------------------------------------

  TStripeBaseObjectList<T> = class(TInterfacedObject, IStripeBaseObjectList<T>)
  strict private
    FItems: TList<T>;
  private
    function GetCount: integer;
  protected
    constructor Create; virtual;
    destructor Destroy; override;
    function CreateObject: T; virtual; abstract;
    function AddObject: T; virtual;
    function GetListID: string; virtual; abstract;
    procedure Clear;
    procedure LoadFromJson(AJson: TJSONObject); virtual; abstract;
    function GetItem(index: integer): T;
    property Count: integer read GetCount;
    property Item[index: integer]: T read GetItem; default;
  end;

//------------------------------------------------------------------------------

  TStripeCharge = class(TStripeBaseObject, IStripeCharge)
  strict private
    FCreated: TDateTime;
    FDesc: string;
    FLiveMode: Boolean;
    FPaid: Boolean;
    FStatus: string;
    FAmountPence: integer;
    FCurrency: TStripeCurrency;
    FRefunded: Boolean;
    FCustomer: string;
    FCard: IStripeCard;
  private
    function GetCreated: TDatetime;
    function GetLiveMode: Boolean;
    function GetPaid: Boolean;
    function GetStatus: string;
    function GetAmountPence: integer;
    function GetCurrency: TStripeCurrency;
    function GetRefunded: Boolean;
    function GetCustomer: string;
    function GetCard: IStripeCard;
    function GetDesc: string;
  protected
    function GetObject: string; override;
    procedure Clear; override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property Created: TDateTime read GetCreated;
    property LiveMode: Boolean read GetLiveMode;
    property Paid: Boolean read GetPaid;
    property Status: string read GetStatus;
    property AmountPence: integer read GetAmountPence;
    property Currency: TStripeCurrency read GetCurrency;
    property Refunded: Boolean read GetRefunded;
    property Customer: string read GetCustomer;
    property Card: IStripeCard read GetCard;
    property Desc: string read GetDesc;
  public
    constructor Create; override;
  end;

//------------------------------------------------------------------------------

  TStripeCard = class(TStripeBaseObject, IStripeCard)
  strict private
    FBrand: string;
    FLast4: string;
    FExpMonth: integer;
    FExpYear: integer;
  private
    function GetBrand: string;
    function GetLast4: string;
    function GetExpMonth: integer;
    function GetExpYear: integer;
  protected
    function GetObject: string; override;
    procedure Clear; override;
    procedure LoadFromJson(AJson: TJsonObject); override;
  public
    property Last4: string read GetLast4;
    property Brand: string read GetBrand;
    property ExpMonth: integer read GetExpMonth;
    property ExpYear: integer read GetExpYear;
  end;

//------------------------------------------------------------------------------

  TStripePlan = class(TStripeBaseObject, IStripePlan)
  strict private
    FInterval: string;
    FName: string;
    FCreated: TDateTime;
    FAmountPence: integer;
    FCurrency: TStripeCurrency;
    FIntervalCount: integer;
    FTrialPeriodDays: integer;
    FStatementDescriptor: string;
  private
    function GetAmountPence: integer;
    function GetCreated: TDateTime;
    function GetCurrency: TStripeCurrency;
    function GetInterval: string;
    function GetIntervalCount: integer;
    function GetName: string;
    function GetStatementDescriptor: string;
    function GetTrialPeriodDays: integer;
  protected
    function GetObject: string; override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property Interval: string read GetInterval;
    property Name: string read GetName;
    property Created: TDateTime read GetCreated;
    property AmountPence: integer read GetAmountPence;
    property Currency: TStripeCurrency read GetCurrency;
    property IntervalCount: integer read GetIntervalCount;
    property TrialPeriodDays: integer read GetTrialPeriodDays;
    property StatementDescriptor: string read GetStatementDescriptor;
  end;

//------------------------------------------------------------------------------

  TStripeSubscription = class(TStripeBaseObject, IStripeSubscription)
  strict private
    FPlan: IStripePlan;
    FStart: TDateTime;
    FStatus: string;
    FCustomer: string;
    FCurrentPeriodStart: TDateTime;
    FCurrentPeriodEnd: TDateTime;
    FEndedAt: TDateTime;
    FTrialStart: TDateTime;
    FTrialEnd: TDateTime;
    FCancelledAt: TDateTime;
    FQuantity: integer;
    FTaxPercent: Single;
  private
    function GetCancelledAt: TDateTime;
    function GetCurrentPeriodEnd: TDateTime;
    function GetCurrentPeriodStart: TDateTime;
    function GetCustomer: string;
    function GetEndedAt: TDateTime;
    function GetPlan: IStripePlan;
    function GetQuantity: integer;
    function GetStart: TDateTime;
    function GetStatus: string;
    function GetTaxPercent: single;
    function GetTrialEnd: TDateTime;
    function GetTrialStart: TDateTime;
  protected
    function GetObject: string; override;
    procedure LoadFromJson(AJson: TJsonObject); override;
  public
    constructor Create; override;
    property Plan: IStripePlan read GetPlan;
    property Start: TDateTime read GetStart;
    property Status: string read GetStatus;
    property Customer: string read GetCustomer;
    property CurrentPeriodStart: TDateTime read GetCurrentPeriodStart;
    property CurrentPeriodEnd: TDateTime read GetCurrentPeriodEnd;
    property EndedAt: TDateTime read GetEndedAt;
    property TrialStart: TDateTime read GetTrialStart;
    property TrialEnd: TDateTime read GetTrialEnd;
    property CancelledAt: TDateTime read GetCancelledAt;
    property Quantity: integer read GetQuantity;
    property TaxPercent: single read GetTaxPercent;
  end;

//------------------------------------------------------------------------------

  TStripeCustomer = class(TStripeBaseObject, IStripeCustomer)
  strict private
    FEmail: string;
    FName: string;
    FDescription: string;
  private
    function GetEmail: string;
    procedure SetEmail(const Value: string);
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    function GetName: string;
    procedure SetName(const Value: string);
  protected
    function GetObject: string; override;
    procedure LoadFromJson(AJson: TJsonObject); override;
    property Name: string read GetName write SetName;
    property Email: string read GetEmail write SetEmail;
    property Description: string read GetDescription write SetDescription;
  public
    procedure Assign(ACustomer: IStripeCustomer);
    procedure Clear; override;
  end;

//------------------------------------------------------------------------------

  TStripeCustomerList = class(TStripeBaseObjectList<IStripeCustomer>, IStripeCustomerList)
  protected
    function CreateObject: IStripeCustomer; override;
    function GetListID: string; override;
    procedure LoadFromJson(AJson: TJsonObject); override;
  end;


//------------------------------------------------------------------------------

  TStripe = class(TInterfacedObject, IStripe)
  strict private
    FSecretKey: string;
    FLastError: string;
    FLastJsonResult: string;
  private
    procedure CheckForError(AJson: TJsonObject);
    {$IFDEF USE_NET_HTTP}
    procedure NetHTTPClient1AuthEvent(const Sender: TObject;
                                      AnAuthTarget: TAuthTargetType;
                                      const ARealm, AURL: string; var AUserName,
                                      APassword: string; var AbortAuth: Boolean;
                                      var Persistence: TAuthPersistenceType);
    function CreateHttp: TNetHTTPClient;
    {$ELSE}
    function CreateHttp: TIdHTTP;
    procedure DoAuthorization(Sender: TObject; Authentication: TIdAuthentication; var Handled: Boolean);
    {$ENDIF}

    function GetHttp(AMethod: string; AParams: TStrings): string;
    function PostHttp(AToken, AMethod: string; AParams: TStrings): string;
    function DeleteHttp(AMethod: string): string;
    function GetLastError: string;
    function GetLastJsonResult: string;

  protected
    function CreateToken(ACardNum: string; AExpMonth, AExpYear: integer; ACvc: string): string;
    function CreateCharge(AToken, ADescription: string;
                          AAmountPence: integer;
                          AMetaData: TStrings;
                          var AError: string;
                          const ACurrency: TStripeCurrency = scGbp): IStripeCharge;
    function RefundCharge(AChargeID: string): string;
    function CreateCard(ACustID, ACardToken: string; var AError: string): IStripeCard; overload;
    function CreateCard(ACustID, ACardNum: string; AExpMonth, AExpYear: integer; ACvc: string; var AError: string): IStripeCard; overload;
    function DeleteCard(ACustID, ACardID: string): Boolean;
    function UpdateDefaultSource(ACustID, ACardID: string): Boolean;
    procedure UpdateCustomerValue(ACustID, AField, AValue: string);
    function CreateChargeForCustomer(ACustID, ADescription: string; AAmountPence: integer; const ACurrency: TStripeCurrency = scGbp): IStripeCharge;
    function GetAccount: string;
    function GetCapabilities: string;
    function GetPersons: string;
    function CustomerExists(ACustID: string; ACustomer: IStripeCustomer): Boolean;
    function GetCustomer(ACustID: string): IStripeCustomer;
    function GetCustomers(const ACreatedAfter: TDateTime  = -1;
                          const ACreatedBefore: TDateTime = -1;
                          const ALimit: Integer = 10): IStripeCustomerList;
    function GetBalance: Extended;
    function CreateCustomer(AEmail, ADescription: string): IStripeCustomer;
    property LastError: string read GetLastError;
    property LastJsonResult: string read GetLastJsonResult;
  public
    constructor Create(ASecretKey: string);
  end;

//------------------------------------------------------------------------------


{$IFNDEF USE_NET_HTTP}
var
  ASsl: TIdSSLIOHandlerSocketOpenSSL;
{$ENDIF}

function  CreateStripe(ASecretKey: string): IStripe;
begin
  Result := TStripe.Create(ASecretKey);
end;

function CreateStripeCustomer: IStripeCustomer;
begin
  Result := TStripeCustomer.Create;
end;

function CurrencyToString(ACurrency: TStripeCurrency): string;
begin
  Result := '';
  case ACurrency of
    scGbp: Result := 'gbp';
    scUsd: Result := 'usd';
  end;
end;

function StringToCurrency(AValue: string): TStripeCurrency;
begin
  AValue := LowerCase(AValue);
  Result := scUnknown;
  if AValue = 'gbp' then Result := scGbp;
  if AValue = 'usd' then Result := scUsd;
end;

//------------------------------------------------------------------------------

{ TStripe }

constructor TStripe.Create(ASecretKey: string);
begin
  inherited Create;
  FSecretKey := ASecretKey;
end;

{$IFNDEF USE_NET_HTTP}

procedure TStripe.DoAuthorization(Sender: TObject; Authentication: TIdAuthentication; var Handled: Boolean);
begin
  Authentication.Username := FSecretKey;
  Authentication.Password := '';
end;

{$ENDIF}

procedure TStripe.CheckForError(AJson: TJsonObject);
var
  AError: TJSONObject;
begin
  FLastError := '';
  {$IFDEF JSONDATAOBJECTS}
  if AJson.Contains('error') then
  begin
    AError := AJson.O['error'] as TJSONObject;
    FLastError := AError.s['message'];
  end;
  {$ELSE}
  if AJson.Values['error'] <> nil then
  begin
    AError := AJson.Values['error'] as TJSONObject;
    FLastError := AError.Values['message'].Value;
  end;
  {$ENDIF}


end;

function TStripe.CreateCard(ACustID, ACardToken: string; var AError: string): IStripeCard;
var
  AParams: TStrings;
  AResult: string;
  AJson: TJSONObject;
begin
  Result := TStripeCard.Create;
  AParams := TStringList.Create;
  try
    AParams.Values['source'] := ACardToken;
    AResult := PostHttp('', C_CUSTOMERS+'/'+ACustID+'/sources',AParams);
    {$IFDEF JSONDATAOBJECTS}
    AJson := TJSONObject.Parse(AResult) as TJSONObject;
    {$ELSE}
    AJson := TJSONObject.ParseJSONValue(AResult) as TJSONObject;
    {$ENDIF}
    try
      CheckForError(AJson);
      AError := FLastError;
      if FLastError <> '' then
        Exit;
      Result.LoadFromJson(AJson);// := // AJson.Values['id'].Value;
    finally
      AJson.Free;
    end;
  finally
    AParams.Free;
  end;
end;

function TStripe.CreateCharge(AToken, ADescription: string; AAmountPence: integer; AMetaData: TStrings; var AError: string; const ACurrency: TStripeCurrency = scGbp): IStripeCharge;
var
  AParams: TStrings;
  AResult: string;
  AJson: TJSONObject;
  ICount: integer;
begin
  Result := TStripeCharge.Create;
  AParams := TStringList.Create;
  try
    AParams.Values['amount'] := IntToStr(AAmountPence);
    AParams.Values['currency'] := CurrencyToString(ACurrency);
    AParams.Values['description'] := ADescription;
    if AMetaData <> nil then
    begin
      for ICount := 0 to AMetaData.Count-1 do
        AParams.Values['metadata['+AMetaData.Names[ICount]+']'] := AMetaData.ValueFromIndex[ICount];
    end;
    AResult := PostHttp(AToken, C_CHARGES, AParams);

    {$IFDEF JSONDATAOBJECTS}
    AJson := TJSONObject.Parse(AResult) as TJSONObject;
    {$ELSE}
    AJson := TJSONObject.ParseJSONValue(AResult) as TJSONObject;
    {$ENDIF}
    try
      CheckForError(AJson);
      AError := FLastError;
      Result.LoadFromJson(AJson);
    finally
      AJson.Free;
    end;
  finally
    AParams.Free;
  end;
end;

function TStripe.CreateChargeForCustomer(ACustID, ADescription: string;
  AAmountPence: integer; const ACurrency: TStripeCurrency): IStripeCharge;
var
  AError: string;
begin
  Result := CreateCharge(ACustID, ADescription, AAmountPence, nil, AError, ACurrency);
end;

{$IFDEF USE_NET_HTTP}

function TStripe.CreateHttp: TNetHTTPClient;
begin
  Result := TNetHTTPClient.Create(nil);
  Result.OnAuthEvent := NetHTTPClient1AuthEvent;
end;

{$ELSE}

function TStripe.CreateHttp: TIdHTTP;
begin
  Result := TIdHTTP.Create(nil);
  Result.IOHandler := ASsl;
  Result.OnAuthorization := DoAuthorization;
end;

{$ENDIF}

function TStripe.CreateToken(ACardNum: string; AExpMonth, AExpYear: integer;
  ACvc: string): string;
var
  AParams: TStrings;
  AResult: string;
  AJson: TJSONObject;
begin
  AParams := TStringList.Create;
  try
    AParams.Values['card[number]'] := ACardNum;
    AParams.Values['card[exp_month]'] := IntToStr(AExpMonth);
    AParams.Values['card[exp_year]'] := IntToStr(AExpYear);
    AParams.Values['card[cvc]'] := ACvc;
    AResult := PostHttp('', C_TOKENS,AParams);
    {$IFDEF JSONDATAOBJECTS}
    AJson := TJSONObject.Parse(AResult) as TJSONObject;
    {$ELSE}
    AJson := TJSONObject.ParseJSONValue(AResult) as TJSONObject;
    {$ENDIF}
    CheckForError(AJson);
    try
      Result := AJson.Values['id'].Value;
    finally
      AJson.Free;
    end;
  finally
    AParams.Free;
  end;
end;

function TStripe.CreateCard(ACustID, ACardNum: string; AExpMonth, AExpYear: integer;
  ACvc: string; var AError: string): IStripeCard;
var
  AParams: TStrings;
  AResult: string;
  AJson: TJSONObject;
begin
  Result := TStripeCard.Create;
  AParams := TStringList.Create;
  try
    AParams.Values['source[object]'] := 'card';

    AParams.Values['source[number]'] := ACardNum;
    AParams.Values['source[exp_month]'] := IntToStr(AExpMonth);
    AParams.Values['source[exp_year]'] := IntToStr(AExpYear);
    AParams.Values['source[cvc]'] := ACvc;
    AResult := PostHttp('', C_CUSTOMERS+'/'+ACustID+'/sources',AParams);
    {$IFDEF JSONDATAOBJECTS}
    AJson := TJSONObject.Parse(AResult) as TJSONObject;
    {$ELSE}
    AJson := TJSONObject.ParseJSONValue(AResult) as TJSONObject;
    {$ENDIF}
    try
      CheckForError(AJson);
      AError := FLastError;
      if FLastError <> '' then
        Exit;
      Result.LoadFromJson(AJson);
    finally
      AJson.Free;
    end;
  finally
    AParams.Free;
  end;
end;

function TStripe.CustomerExists(ACustID: string; ACustomer: IStripeCustomer): Boolean;
var
  ACust: IStripeCustomer;
begin
  ACust := GetCustomer(ACustID);
  Result := ACust.ID <> '';
  if (Result) and (ACustomer <> nil) then
    ACustomer.Assign(ACust);
end;

function TStripe.DeleteCard(ACustID, ACardID: string): Boolean;
var
  AResult: string;
begin
  AResult := DeleteHttp('customers/'+ACustID+'/sources/'+ACardID);
  Result := True;
end;

function TStripe.GetAccount: string;
begin
  Result := GetHttp(C_ACCOUNT, nil);
end;

function TStripe.GetBalance: Extended;
var
  AResult: string;
begin
  Result := 0;
  AResult := GetHttp(C_BALANCE, nil);
end;

function TStripe.GetCapabilities: string;
begin
  Result := GetHttp('accounts/acct_1Ei2lxJ9jnzBMBPN/capabilities', nil);
end;

function TStripe.GetCustomer(ACustID: string): IStripeCustomer;
var
  AResult: string;
  AJson: TJSONObject;
begin
  Result := TStripeCustomer.Create;
  AResult := GetHttp(C_CUSTOMERS+'/'+ACustID, nil);
  {$IFDEF JSONDATAOBJECTS}
  AJson := TJSONObject.Parse(AResult) as TJSONObject;
  {$ELSE}
  AJson := TJSONObject.ParseJSONValue(AResult) as TJSONObject;
  {$ENDIF}
  try
    CheckForError(AJson);
    if FLastError <> '' then
    begin
      Result.Clear;
      Exit;
    end;
    {$IFDEF JSONDATAOBJECTS}
    if AJson.S['deleted'] = 'true' then
    {$ELSE}
    if AJson.Values['deleted'].Value = 'true' then
    {$ENDIF}
    begin
      Result.Clear;
      Exit;
    end;
    Result.LoadFromJson(AJson);
  finally
    AJson.Free;
  end;
end;

function TStripe.GetCustomers(const ACreatedAfter: TDateTime = -1;
                              const ACreatedBefore: TDateTime = -1;
                              const ALimit: Integer = 10): IStripeCustomerList;
var
  AResult: string;
  AJson: TJSONObject;
  AParams: TStrings;
begin
  AParams := TStringList.Create;
  try
    if ACreatedAfter > -1 then AParams.Values['created[gt]'] := IntToStr(DateTimeToUnix(ACreatedAfter));
    if ACreatedBefore > -1 then AParams.Values['created[lt]'] := IntToStr(DateTimeToUnix(ACreatedBefore));
    AParams.Values['limit'] := IntToStr(ALimit);

    Result := TStripeCustomerList.Create;
    AResult := GetHttp(C_CUSTOMERS, AParams);
    {$IFDEF JSONDATAOBJECTS}
    AJson := TJSONObject.Parse(AResult) as TJSONObject;
    {$ELSE}
    AJson := TJSONObject.ParseJSONValue(AResult) as TJSONObject;
    {$ENDIF}
    try
      Result.LoadFromJson(AJson);
    finally
      AJson.Free;
    end;
  finally
    AParams.Free;
  end;
end;

function TStripe.GetHttp(AMethod: string; AParams: TStrings): string;

  function ParamsToUrl(AStrings: TStrings): string;
  var
    ICount: integer;
  begin
    Result := '';
    for ICount := 0 to AStrings.Count-1 do
    begin
      Result := Result + AStrings[ICount];
      if ICount < AStrings.Count-1 then
        Result := Result + '&';
    end;
  end;

var
  {$IFDEF USE_NET_HTTP}
  AHttp: TNetHTTPClient;
  AResponse: IHTTPResponse;
  {$ELSE}
  AHttp: TIdHTTP;
  {$ENDIF}

  AUrl: string;
begin
  AHttp := CreateHttp;
  try
    AUrl := 'https://api.stripe.com/v1/'+AMethod;
    if AParams <> nil then
    begin
      if AParams.Count > 0 then
        AUrl := AUrl + '?'+ParamsToUrl(AParams);
    end;

    {$IFDEF USE_NET_HTTP}
    AHttp.CustomHeaders['Authorization'] := 'Bearer '+FSecretKey;
    AResponse := AHttp.Get(AUrl);
    Result := AResponse.ContentAsString;
    FLastJsonResult := Result;
    {$ELSE}
    AHttp.Request.CustomHeaders.AddValue('Authorization', 'Bearer '+FSecretKey);
    Result := AHttp.Get(AUrl);
    {$ENDIF}
  finally
    AHttp.Free;
  end;
end;

function TStripe.GetLastError: string;
begin
  Result := FLastError;
end;

function TStripe.GetLastJsonResult: string;
begin
  Result := FLastJsonResult;
end;

function TStripe.GetPersons: string;
begin
 Result := GetHttp('accounts/acct_1Ehfk3C0L5u7blDo/persons', nil);
end;

function TStripe.PostHttp(AToken, AMethod: string; AParams: TStrings): string;
var
  {$IFDEF USE_NET_HTTP}
  AHttp: TNetHTTPClient;
  AResponse: IHTTPResponse;
  {$ELSE}
  AHttp: TIdHTTP;
  AResponse: TStringStream;
  {$ENDIF}
begin
  AHttp := CreateHttp;
  try
    if AToken <> '' then
    begin
      if Pos('tok_', AToken) = 1 then AParams.Values['source'] := AToken;
      if Pos('cus_', AToken) = 1 then AParams.Values['customer'] := AToken;
    end;
    {$IFDEF USE_NET_HTTP}
    AHttp.CustomHeaders['Authorization'] := 'Bearer '+FSecretKey;
    AResponse := AHttp.Post('https://api.stripe.com/v1/'+AMethod, AParams);
    Result := AResponse.ContentAsString;
    FLastJsonResult := Result;
    {$ELSE}
    AHttp.Request.CustomHeaders.AddValue('Authorization', 'Bearer '+FSecretKey);
    AResponse := TStringStream.Create;
    try
      AHttp.Post('https://api.stripe.com/v1/'+AMethod, AParams, AResponse);
      Result := AResponse.DataString;
    finally
      AResponse.Free;
    end;
    {$ENDIF}
  finally
    AHttp.Free;
  end;
end;

function TStripe.RefundCharge(AChargeID: string): string;
var
  AParams: TStrings;
begin
  try
    AParams := TStringList.Create;
    try
      AParams.Values['charge'] := AChargeID;
      Result := PostHttp('', 'refunds', AParams);
    finally
      AParams.Free;
    end;
  except
    on E:Exception do
      Result := E.Message;
  end;
end;

procedure TStripe.UpdateCustomerValue(ACustID, AField, AValue: string);
var
  AParams: TStrings;
  AResult: string;
  AJson: TJSONObject;
begin
  AParams := TStringList.Create;
  try
    AParams.Values[AField] := AValue;
    AResult := PostHttp('', C_CUSTOMERS+'/'+ACustID, AParams);
    {$IFDEF JSONDATAOBJECTS}
    AJson := TJSONObject.Parse(AResult) as TJSONObject;
    {$ELSE}
    AJson := TJSONObject.ParseJSONValue(AResult) as TJSONObject;
    {$ENDIF}
    try
      CheckForError(AJson);
    finally
      AJson.Free;
    end;
  finally
    AParams.Free;
  end;
end;

function TStripe.UpdateDefaultSource(ACustID, ACardID: string): Boolean;
var
  AParams: TStrings;
begin
  try
    AParams := TStringList.Create;
    try
      AParams.Values['default_source'] := ACardID;
      PostHttp('', 'customers/'+ACustID, AParams);
      Result := True;
    finally
      AParams.Free;
    end;
  except
    Result := False;
  end;
end;

function TStripe.DeleteHttp(AMethod: string): string;
var
  {$IFDEF USE_NET_HTTP}
  AHttp: TNetHTTPClient;
  AResponse: IHTTPResponse;
  {$ELSE}
  AHttp: TIdHTTP;
  {$ENDIF}
begin
  AHttp := CreateHttp;
  try
    {$IFDEF USE_NET_HTTP}
    AHttp.CustomHeaders['Authorization'] := 'Bearer '+FSecretKey;
    AResponse := AHttp.Delete('https://api.stripe.com/v1/'+AMethod);
    Result := AResponse.ContentAsString
    {$ELSE}

    {$ENDIF}
  finally
    AHttp.Free;
  end;
end;

function TStripe.CreateCustomer(AEmail, ADescription: string): IStripeCustomer;
var
  AParams: TStrings;
  AResult: string;
  AJson: TJSONObject;
begin
  Result := TStripeCustomer.Create;
  AParams := TStringList.Create;
  try
    AParams.Values['email'] := AEmail;
    AParams.Values['description'] := ADescription;
    AResult := PostHttp('', C_CUSTOMERS, AParams);
    {$IFDEF JSONDATAOBJECTS}
    AJson := TJSONObject.Parse(AResult) as TJSONObject;
    {$ELSE}
    AJson := TJSONObject.ParseJSONValue(AResult) as TJSONObject;
    {$ENDIF}
    try
      CheckForError(AJson);
      Result.LoadFromJson(AJson);
    finally
      AJson.Free;
    end;
  finally
    AParams.Free;
  end;
end;


{$IFDEF USE_NET_HTTP}

procedure TStripe.NetHTTPClient1AuthEvent(const Sender: TObject;
  AnAuthTarget: TAuthTargetType; const ARealm, AURL: string; var AUserName,
  APassword: string; var AbortAuth: Boolean;
  var Persistence: TAuthPersistenceType);
begin
  if AnAuthTarget = TAuthTargetType.Server then
  begin
    AUserName := FSecretKey;
    APassword := '';
  end;
end;

{$ENDIF}
//------------------------------------------------------------------------------

{ TStripeCard }


procedure TStripeCard.Clear;
begin
  inherited;
  FBrand := '';
  FLast4 := '';
  FExpMonth := 0;
  FExpYear := 0;
end;

function TStripeCard.GetBrand: string;
begin
  Result := FBrand;
end;

function TStripeCard.GetExpMonth: integer;
begin
  Result :=FExpMonth;
end;

function TStripeCard.GetExpYear: integer;
begin
  Result := FExpYear;
end;

function TStripeCard.GetLast4: string;
begin
  Result := FLast4;
end;

function TStripeCard.GetObject: string;
begin
  Result := C_CARD;
end;

procedure TStripeCard.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  {$IFDEF JSONDATAOBJECTS}
  FBrand := AJson.S['brand'];
  FLast4 := AJson.S['last4'];
  FExpMonth := AJson.I['exp_month'];
  FExpYear := AJson.I['exp_year'];
  {$ELSE}
  FBrand := AJson.Values['brand'].Value;
  FLast4 := AJson.Values['last4'].Value;
  FExpMonth := StrToInt(AJson.Values['exp_month'].Value);
  FExpYear := StrToInt(AJson.Values['exp_year'].Value);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

{ TStripeBaseObject }

procedure TStripeBaseObject.Clear;
begin
  // overridden in descendant objects.
end;

constructor TStripeBaseObject.Create;
begin
  FId := '';
end;

function TStripeBaseObject.GetID: string;
begin
  Result := FId;
end;

procedure TStripeBaseObject.LoadFromJson(AJson: TJsonObject);
begin
  FJson := AJson;
  {$IFDEF JSONDATAOBJECTS}
  FId := FJson.S['id'];
  {$ELSE}
  FId := FJson.Values['id'].Value;
  {$ENDIF}
end;
 procedure TStripeBaseObject.SetID(const Value: string);
begin
  FId := Value;
end;

//------------------------------------------------------------------------------

{ TStripeCustomer }

procedure TStripeCustomer.Assign(ACustomer: IStripeCustomer);
begin
  inherited;
  ID := ACustomer.ID;
  Description := ACustomer.Description;
  Email := ACustomer.Email;
end;

procedure TStripeCustomer.Clear;
begin
  inherited;
  FEmail := '';
  FDescription := '';
  FName := '';
end;

function TStripeCustomer.GetDescription: string;
begin
  Result := FDescription;
end;

function TStripeCustomer.GetEmail: string;
begin
  Result := FEmail;
end;

function TStripeCustomer.GetName: string;
begin
  Result := FName;
end;

function TStripeCustomer.GetObject: string;
begin
  Result := C_CUSTOMER;
end;

procedure TStripeCustomer.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  Clear;
  {$IFDEF JSONDATAOBJECTS}
  if AJson.Types['description'] <> jdtObject then FDescription := AJson.S['description'];
  if AJson.Types['email'] <> jdtObject then FEmail := AJson.S['email'];
  if AJson.Types['name'] <> jdtObject then FName := AJson.S['name'];
  {$ELSE}
  FDescription := AJson.Values['description'].Value;
  FEmail := AJson.Values['email'].Value;
  FName := AJson.Values['name'].Value;
  {$ENDIF}
end;


procedure TStripeCustomer.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TStripeCustomer.SetEmail(const Value: string);
begin
  FEmail := Value;
end;

procedure TStripeCustomer.SetName(const Value: string);
begin
  FName := Value;
end;

//------------------------------------------------------------------------------

{ TStripePlan }

function TStripePlan.GetAmountPence: integer;
begin
  Result := FAmountPence;
end;

function TStripePlan.GetCreated: TDateTime;
begin
  Result := FCreated;
end;

function TStripePlan.GetCurrency: TStripeCurrency;
begin
  Result := FCurrency;
end;

function TStripePlan.GetInterval: string;
begin
  Result := FInterval;
end;

function TStripePlan.GetIntervalCount: integer;
begin
  Result := FIntervalCount;
end;

function TStripePlan.GetName: string;
begin
  Result := FName;
end;

function TStripePlan.GetObject: string;
begin
  Result := C_PLAN;
end;

function TStripePlan.GetStatementDescriptor: string;
begin
  Result := FStatementDescriptor;
end;

function TStripePlan.GetTrialPeriodDays: integer;
begin
  Result := FTrialPeriodDays;
end;

procedure TStripePlan.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  {$IFDEF JSONDATAOBJECTS}
  FInterval := AJson.S['interval'];
  FName := AJson.S['name'];
  FCreated := UnixToDateTime(StrToInt(AJson.S['created']));
  FAmountPence := StrToIntDef(AJson.S['amount'], 0);
  FCurrency :=  StringToCurrency(AJson.S['currency']);
  FIntervalCount := StrToIntDef(AJson.S['interval_count'], 0);
  FTrialPeriodDays := StrToIntDef(AJson.S['trial_period_days'], 0);
  FStatementDescriptor := AJson.S['statement_descriptor'];
  {$ELSE}
  FInterval := AJson.Values['interval'].Value;
  FName := AJson.Values['name'].Value;
  FCreated := UnixToDateTime(StrToInt(AJson.Values['created'].Value));
  FAmountPence := StrToIntDef(AJson.Values['amount'].Value, 0);
  FCurrency :=  StringToCurrency(AJson.Values['currency'].Value);
  FIntervalCount := StrToIntDef(AJson.Values['interval_count'].Value, 0);
  FTrialPeriodDays := StrToIntDef(AJson.Values['trial_period_days'].Value, 0);
  FStatementDescriptor := AJson.Values['statement_descriptor'].Value;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

{ TStripeSubscription }

constructor TStripeSubscription.Create;
begin
  inherited;
  FPlan := TStripePlan.Create;
end;

function TStripeSubscription.GetCancelledAt: TDateTime;
begin
  Result := FCancelledAt;
end;

function TStripeSubscription.GetCurrentPeriodEnd: TDateTime;
begin
  Result := FCurrentPeriodEnd;
end;

function TStripeSubscription.GetCurrentPeriodStart: TDateTime;
begin
  Result := FCurrentPeriodStart;
end;

function TStripeSubscription.GetCustomer: string;
begin
  Result := FCustomer;
end;

function TStripeSubscription.GetEndedAt: TDateTime;
begin
  Result := FEndedAt;
end;

function TStripeSubscription.GetObject: string;
begin
  Result := C_SUBSCRIPTION;
end;

function TStripeSubscription.GetPlan: IStripePlan;
begin
  Result := FPlan;
end;

function TStripeSubscription.GetQuantity: integer;
begin
  Result := FQuantity;
end;

function TStripeSubscription.GetStart: TDateTime;
begin
  Result := FStart;
end;

function TStripeSubscription.GetStatus: string;
begin
  Result := FStatus;
end;

function TStripeSubscription.GetTaxPercent: single;
begin
  Result := FTaxPercent;
end;

function TStripeSubscription.GetTrialEnd: TDateTime;
begin
  Result := FTrialEnd;
end;

function TStripeSubscription.GetTrialStart: TDateTime;
begin
  Result := FTrialStart;
end;

procedure TStripeSubscription.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  {$IFDEF JSONDATAOBJECTS}
  FPlan.LoadFromJson(AJson.O['plan']);
  FStatus := AJson.S['status'];
  {$ELSE}
  FPlan.LoadFromJson(AJson.Values['plan'] as TJSONObject);
  FStatus := AJson.Values['status'].Value;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

{ TStripeBaseObjectList<T> }

function TStripeBaseObjectList<T>.AddObject: T;
begin
  Result := CreateObject;
  FItems.Add(Result);
end;

procedure TStripeBaseObjectList<T>.Clear;
begin
  FItems.Clear;
end;

constructor TStripeBaseObjectList<T>.Create;
begin
  FItems := TList<T>.Create;
end;

destructor TStripeBaseObjectList<T>.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TStripeBaseObjectList<T>.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TStripeBaseObjectList<T>.GetItem(index: integer): T;
begin
  Result := FItems[index];
end;


//------------------------------------------------------------------------------

{ TStripeCustomerList }

function TStripeCustomerList.CreateObject: IStripeCustomer;
begin
  Result := TStripeCustomer.Create;
end;

function TStripeCustomerList.GetListID: string;
begin
  Result := C_CUSTOMERS;
end;

procedure TStripeCustomerList.LoadFromJson(AJson: TJsonObject);
var
  AArray: TJSONArray;
  ICount: integer;
begin
  Clear;
  if AJson = nil then
    Exit;
  {$IFDEF JSONDATAOBJECTS}
  AArray := AJson.A['data'];
  {$ELSE}
  AArray := AJson.Values['data'] as TJSONArray;
  {$ENDIF}

  for ICount := 0 to AArray.Count-1 do
  begin
    {$IFDEF JSONDATAOBJECTS}
    AddObject.LoadFromJson(AArray.O[ICount]);
    {$ELSE}
    AddObject.LoadFromJson(AArray.Items[ICount] as TJSONObject);
    {$ENDIF}
  end;
end;

//------------------------------------------------------------------------------

{ TStripeCharge }

procedure TStripeCharge.Clear;
begin
  inherited;
  FCreated := 0;
  FDesc := '';
  FLiveMode := False;
  FPaid := False;
  FStatus := '';
  FAmountPence := 0;
  FCurrency := scUnknown;
  FRefunded := False;
  FCustomer := '';
  FCard.Clear;
end;

constructor TStripeCharge.Create;
begin
  inherited;
  FCard := TStripeCard.Create;
end;

function TStripeCharge.GetAmountPence: integer;
begin
  Result := FAmountPence;
end;

function TStripeCharge.GetCurrency: TStripeCurrency;
begin
  Result := FCurrency;
end;

function TStripeCharge.GetCustomer: string;
begin
  Result := FCustomer;
end;

function TStripeCharge.GetDesc: string;
begin
  Result := FDesc;
end;

function TStripeCharge.GetCard: IStripeCard;
begin
  Result := FCard;
end;

function TStripeCharge.GetCreated: TDatetime;
begin
  Result := FCreated;
end;

function TStripeCharge.GetLiveMode: Boolean;
begin
  Result := FLiveMode;
end;

function TStripeCharge.GetObject: string;
begin
  Result := C_CHARGE;
end;

function TStripeCharge.GetPaid: Boolean;
begin
  Result := FPaid;
end;

function TStripeCharge.GetRefunded: Boolean;
begin
  Result := FRefunded;
end;

function TStripeCharge.GetStatus: string;
begin
  Result := FStatus;
end;

procedure TStripeCharge.LoadFromJson(AJson: TJsonObject);
begin
  inherited;
  Clear;

  FCard := TStripeCard.Create;

  {$IFDEF JSONDATAOBJECTS}
  FCreated := UnixToDateTime(AJson.I['created']);
  FLiveMode := AJson.S['livemode'] = 'true';
  FPaid := AJson.S['paid'] = 'true';
  FStatus := AJson.S['status'];
  FAmountPence := StrToIntDef(AJson.S['amount'], 0);
  FCurrency := StringToCurrency(AJson.S['currency']);
  FRefunded := AJson.S['refunded'] = 'true';
  FCustomer := AJson.S['customer'];
  if AJson.Types['description'] = jdtString then FDesc := AJson.S['description'];
  FCustomer := AJson.S['customer'];
  if AJson.O['payment_method_details'] <> nil then
  begin
    if AJson.O['payment_method_details'].O['card'] <> nil then
      FCard.LoadFromJson(AJson.O['payment_method_details'].O['card']);
  end;
  {$ELSE}
  FCreated := UnixToDateTime(StrToInt(AJson.Values['created'].Value));
  FLiveMode := AJson.Values['livemode'].Value = 'true';
  FPaid := AJson.Values['paid'].Value = 'true';
  FStatus := AJson.Values['status'].Value;
  FAmountPence := StrToIntDef(AJson.Values['amount'].Value, 0);
  FCurrency := StringToCurrency(AJson.Values['currency'].Value);
  FRefunded := AJson.Values['refunded'].Value = 'true';
  FCustomer := AJson.Values['customer'].Value;

  AMethod := AJson.Values['payment_method_details'] as TJSONObject;
  if AMethod <> nil then
  begin
    if AMethod.Values['card'] as TJSONObject <> nil then
      FCard.LoadFromJson(AMethod.Values['card'] as TJSONObject);
  end;
  {$ENDIF}



end;


initialization

  {$IFNDEF USE_NET_HTTP}
  ASsl := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  ASsl.SSLOptions.SSLVersions := [sslvTLSv1_2];
  {$ENDIF}

finalization

  {$IFNDEF USE_NET_HTTP}
  ASsl.Free;
  {$ENDIF}

end.
