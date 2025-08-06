unit UChamarArquivoJSON;

interface
uses UDisciplinas, UEstudantes, UProfessor, UTurmas, UMatriculas;
   type TLista_JSON = class
     public
     procedure puxar_json;
   end;
implementation
procedure TLista_JSON.puxar_json;
  begin
    writeln('puxar o json ' + 'usar o json ');
  end;
end.
