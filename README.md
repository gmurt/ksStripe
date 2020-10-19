# ksStripe
Stripe interface for Delphi

It's not complete, I'm just adding to it as I need it for one of my projects.  If you'd like me to add support for particular Stripe functionality, let me know ;-)

Through conditional defines, it should detect if you are using XE8 or later and use the TNetHttpClient component.  If earlier than XE8, it will switch to TIdHttp.  

Note. If using Indy, it will require the OpenSSL's to be available to the application, ideally in the .exe folder.  (libeay32.dll and ssleay32.dll)

#### Example use

To return a list of stripe customers and populate a listbox...

```
uses ksStripe
...
var
  AStripe: IStripe;
  ACustomers: IStripeCustomerList;
  ACus: IStripeCustomer;
  ICount: integer;
begin
  AStripe := CreateStripe('sk_live_*********);
  ACustomers := AStripe.GetCustomers(0, Now, 100);
  for ICount := 0 to ACustomers.Count-1 do
  begin
    ACus := ACustomers[Icount];
    ListBox1.Items.Add(ACus.ID+' '+ACus.Name+' '+ACus.Email); 
  end;
end;
 ```
