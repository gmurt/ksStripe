unit untMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm10 = class(TForm)
    lbCustomers: TListBox;
    btnGetCustomers: TButton;
    procedure btnGetCustomersClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

uses ksStripe;

const
  C_STRIPE_SECRET_KEY = 'sk_live_************';


{$R *.dfm}

procedure TForm10.btnGetCustomersClick(Sender: TObject);
var
  AStripe: IStripe;
  ACustomers: IStripeCustomerList;
  ACus: IStripeCustomer;
  ICount: integer;
begin
  if Pos('*', C_STRIPE_SECRET_KEY) > 0 then
  begin
    ShowMessage('Please replace the C_STRIPE_SECRET_KEY const in untMain.pas with your Stripe API Secret Key.');
    Exit;
  end;

  AStripe := CreateStripe(C_STRIPE_SECRET_KEY);
  ACustomers := AStripe.GetCustomers(0, Now, 100);
  for ICount := 0 to ACustomers.Count-1 do
  begin
    ACus := ACustomers[Icount];
    lbCustomers.Items.Add(ACus.ID+' '+ACus.Name+' '+ACus.Email);
  end;
end;


end.
