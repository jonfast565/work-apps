with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO;

package body LinkedList is
   procedure Print (List : Node_Access) is
   begin
      declare
         Iterator : Node_Access := List;
      begin
         while Iterator /= null loop
            Ada.Text_IO.Unbounded_IO.Put_Line (Iterator.Value);
            Iterator := Iterator.Next;
         end loop;
      end;
   end Print;

   procedure Insert (List : Node_Access; Index : Integer; String : Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      declare
         Iterator: Node_Access := List;
         Node_Next: Node_Access;
         New_Node: Node_Access;
         Node_Previous: Node_Access;
         Counter: Integer := 0;
      begin
         while Iterator.Next /= null loop
            Iterator := Iterator.Next;
            Counter := Counter + 1;
            exit when Counter >= Index;
         end loop;
         Node_Previous := Iterator;
         Node_Next := Iterator.Next;
         New_Node := new Node'(Value => String, Next => Node_Next);
         Node_Previous.Next := New_Node;
      end;
   end Insert;

   procedure Insert (List : Node_Access; Node : Node_Access) is
   begin
      declare
         Iterator: Node_Access := List;
      begin
         while Iterator.Next /= null loop
            Iterator := Iterator.Next;
         end loop;
         Iterator.Next := Node;
      end;
   end Insert;

   procedure Update
     (List         : Node_Access; Index : Integer;
      Value_String : Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      declare
         Iterator: Node_Access := List;
         Counter: Integer := 0;
      begin
      while Iterator.Next /= null loop
            Iterator := Iterator.Next;
            Counter := Counter + 1;
            exit when Counter >= Index;
         end loop;
         Iterator.Value := Value_String;
      end;
   end Update;

   function Delete (List : Node_Access; Index : Integer) return Node_Access is
   begin
      declare
         Iterator: Node_Access := List;
         Node_Next: Node_Access;
         Node_Previous: Node_Access;
         Counter: Integer := 0;
      begin
         while Iterator.Next /= null loop
            Iterator := Iterator.Next;
            Counter := Counter + 1;
            exit when Counter >= Index;
         end loop;
         Node_Previous := Iterator;
         Node_Next := Iterator.Next.Next;
         Node_Previous.Next := Node_Next;
      end;
   end Delete;
end LinkedList;
