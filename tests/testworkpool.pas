unit TestWorkPool;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Manhuard.WorkPool;

type

  { TMyWork }

  TMyWork = class(specialize TWork<string>)
  private
    FCount: Integer;
  protected
    property Count: Integer read FCount;
  public
    function Execute: string; override;
    constructor Create;
    destructor Destroy; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure MyWorkSuccess(Sender: TMyWork.TSelf; Return: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  Work: TMyWork;
begin
  Work := TMyWork.Create;
  Work.OnSuccess := @MyWorkSuccess;
  WorkPool.Exec(Work);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Append(Format('Worker Count: %d', [WorkPool.WorkerCount]));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Append(Format('Work Queue: %d', [WorkPool.WorkQueue.Count]));
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  WorkPool.ReduceWorkers(1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.Append(Format('Pool capacity: %d', [WorkPool.Capacity]));
end;

procedure TForm1.MyWorkSuccess(Sender: TMyWork.TSelf; Return: string);
begin
  raise Exception.Create('sadsad');
  Memo1.Append(Return);
end;

{ TMyWork }

function TMyWork.Execute: string;
var
  i: Integer;
begin

  for i in [1..6] do
  begin
    Sleep(500);
    FCount := i;
    WriteLn(i);
  end;
  Result := 'MyWork';
end;

constructor TMyWork.Create;
begin
  //WriteLn('Work Created');
end;

destructor TMyWork.Destroy;
begin
  //WriteLn('Work Destroyed');
end;

end.

