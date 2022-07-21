#include <iostream>
#include<string>
#include<iomanip>
#include <fstream>
//#include <cstring>

using namespace std;

void initial()
{
	/*
ifstream read ("practice.txt");
ofstream write("practice1.txt");
string a;
while(getline(read,a))
    {
        write<<a<<endl;
    }


    
    ifstream read;
ofstream write("practice1.txt");
string line="haider";
int c = 0;

    write<<line;
*/
    char yes_no ;
    cout << "Do you already have zambeel account?(Y/N) " ;
    cin >> yes_no ;

    string name,login, password ;

    int y_n = 0 ;
    if (yes_no == 'N')
    {
       cout << "Enter your university roll number as login: " ;
       cin >> login;
       cout << endl ;
       //string read, write;
       ifstream read((login + ".txt").c_str());

        //create new user file

       cout << "Enter password: " ;
       cin >> password ;
       //write in users file
       ofstream write ((login + ".txt").c_str());
       string line = password;
       write << line;

       cout << "Congratulations ! Your account has been created. ";
    

    }
    else if (yes_no == 'Y')
    {
       string in_file_password ;

       cout << "Enter your university roll number as login: " ;
       cin >>  login ;
       cout << endl ;
        

       //search_file
       fstream read((login+".txt").c_str());
       read >> in_file_password;
	   //getline(read , in_file_password);
	   //match the password
       do
       {
	   	cout << "Please Enter Password: " ;
       	cin >> password ;
	   }
	   while (password!= in_file_password);
	   /*str:
cout << "Enter username: ";
cin >> username;
cout << endl;
cout << "Enter passward: ";
cin >> passward;

int counter = 0;
string temp_name;
string temp_passward;
for (int i = 6; i < line.length(); i++)
{
if (line[i] != ' ')
{
temp_name = temp_name + line[i];
}
else
{
counter = i;
break;
}

}

if (temp_name == username)
{
for (int i = counter + 1; i < line.length(); i++)
{
temp_passward = temp_passward + line[i];
}

if (temp_passward == passward)
{
cout << "LOgin Successful" << endl;
}
else
{
cout << "Passward Incorrect" << endl;
cout << "Enter again" << endl;
goto str;
}
}
else
{
cout << "wrong Username" << endl;
cout << "Enter again" << endl;
goto str;
}
}
else
{
read.close();
cout << "SIGN UP" << endl;
cout << "Enter your username you want to use: ";
cin >> username;
above:
cout << "Enter passward :";
cin >> passward;

while (passward.length() < 8)
{
cout << " Password is too short! " << endl;
cout << " Enter Password Again! " << endl;
cin >> passward;
}
for (int i = 0; i < passward.length(); i++)
{
for (int j = 0; j < 9; j++)
{
if (passward[i] - 48 == j)
{
cout << " Thankyou for signup! " << endl;
goto below;
}
}
}
cout << " Enter password with atleast 1 digit! " << endl;
goto above;
below:
int siz = passward.length();

string temp;
temp = "Admin";
temp = temp + ' ';
for (int i = 0; i < username.length(); i++)
{
temp = temp + username[i];
}
temp = temp + ' ';
for (int i = 0; i < passward.length(); i++)
{
temp = temp + passward[i];
}*/
       

        /*ifstream in_file;
        in_file.open("C:\\Users\\Safiullah\\Desktop\\login.txt");
        in_file >> in_file_password ;

        if (password != in_file_password)
        {
            do
            {
                cout << "Please Enter correct password: " ;
                cin >> password ;
            }
            while(password != in_file_password) ;
        }*/


       //ifstream in_file("C:\\Users\\Safiullah\\Desktop\\login.txt");

    }



}


void course_reader()
{
    string sub, day, starting, ending  ;
    int serial_no = 0 ;

    ifstream in_file;
    in_file.open("practice.txt");

    for(int i =1 ; i < 9  ; i++)
    {
        in_file >> serial_no >> sub >> day >> starting >>  ending ;

        //cout << "Hello world!" << endl ;
        if(i%2 == 0)
        {
            cout << setw(30) << serial_no << " " << sub << " " << day << " " << starting << " - "<< ending << endl;
        }
        else
        {
            cout << serial_no << " " << sub << " " << day << " " << starting << " - "<< ending ;

        }
    }

    cout << endl << endl ;

    //Asking a student to select the courses

    int no_of_sub = 0 ;

    cout << "You can take 6 courses at maximum. How many courses do you want to take this semester ?  " ;
    cin >> no_of_sub ;

    for(int i = 1 ; i <= no_of_sub ; i++)
    {
        cout << "Select your course number " << i << "  :  " ;
        cin >> serial_no ;

        //match the serial numbers in courses file

        //copy all the course information from courses file into the user's file

        //show the courses in shopping cart

    }


}






int main()
{

    initial() ;


    return 0;
}


