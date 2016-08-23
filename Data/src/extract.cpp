#include<iostream>
#include<fstream>
#include<string>
#include<cstdlib>
#include<vector>
using namespace std;

float a[8][8];
float b[11][16];
float c[11][16];
string prov[11]={"Artibonite","Centre","Grand'Anse","Nippes","Nord","Nord-Ouest","Nord-Est",
"Ouest","Port-au-Prince","Sud","Sud-Est"};
void input_a(int row,string str)
{
   vector<int> apos_a;
   for (int i=0;i<str.length();i++)
   {
       if (str[i]==',')
       {
           apos_a.push_back(i);
       }
   }
   int j=0;
   for (int i=0;i<apos_a.size()-1;i++)
   {
       if (apos_a[i]!=(apos_a[i+1]-1))
       {
            string p=str.substr(apos_a[i]+1,apos_a[i+1]-apos_a[i]-1);
            if (p[0]=='\"')
            {
                  p=p.substr(1,p.length()-1);
            }
            a[row][j]=atof(p.c_str());
            j++;
       }
   }
   if (apos_a.size()==16)
   {
       a[row][0]=a[row][0]+a[row][1]/10;
       a[row][1]=a[row][2]+a[row][3]/10;
       a[row][2]=a[row][4]+a[row][5]/10;
       a[row][3]=a[row][6]+a[row][7]/10;
   }
}

void input_b(int row,string str)
{
   vector<int> apos_b;
   for (int i=0;i<str.length();i++)
   {
       if (str[i]==',' || str[i]=='(' || str[i]==')')
       {
           apos_b.push_back(i);
       }
   }
   apos_b.push_back(str.length());
   int j=0;
   for (int i=0;i<apos_b.size()-1;i++)
   {
       if (apos_b[i]!=(apos_b[i+1]-1))
       {
            string p=str.substr(apos_b[i]+1,apos_b[i+1]-apos_b[i]-1);
            if (p[0]=='\"')
            {
                  p=p.substr(1,p.length()-1);
            }
            b[row][j]=atof(p.c_str());
            j++;
       }
   }
   if (apos_b.size()==27)
   {
       b[row][12]=b[row][12]+b[row][13]/10;
       b[row][13]=b[row][14]+b[row][15]/10;
   }
}

void input_c(int row,string str)
{
   vector<int> apos_c;
   for (int i=0;i<str.length();i++)
   {
       if (str[i]==',' || str[i]=='(' || str[i]==')')
       {
           apos_c.push_back(i);
       }
   }
   apos_c.push_back(str.length());
   int j=0;
   for (int i=0;i<apos_c.size()-1;i++)
   {
       if (apos_c[i]!=(apos_c[i+1]-1))
       {
            string p=str.substr(apos_c[i]+1,apos_c[i+1]-apos_c[i]-1);
            if (p[0]=='\"')
            {
                  p=p.substr(1,p.length()-1);
            }
            c[row][j]=atof(p.c_str());
            j++;
       }
   }
   if (apos_c.size()==27)
   {
       c[row][12]=c[row][12]+c[row][13]/10;
       c[row][13]=c[row][14]+c[row][15]/10;
   }
}

string transform_date(string a, string b, string c)
{
    string out=c+"-";
    if (b=="novembre")
        out+="11";
    else if (b=="avril")
        out+="04";
    else if (b[0]=='a')
        out+="08";
    else if (b[0]=='d')
        out+="12";
    else if (b[0]=='f')
        out+="02";
    else if (b=="janvier")
        out+="01";
    else if (b=="juillet")
        out+="07";
    else if (b=="juin")
        out+="06";
    else if (b=="mai")
        out+="05";
    else if (b=="mars")
        out+="03";
    else if (b=="octobre")
        out+="10";
    else if (b=="septembre")
        out+="09";
    else
        cout<<"Cannot identify month!"<<endl;
    out+="-";
    if (a.size()<2) out+='0';
    out+=a;
    return out;
}

int main()
{
    string fname;
    ifstream flst("list.txt");
    ofstream ofs("cholera5+.txt");
    ofstream ofs2("cholera.txt");
    ofs<<"date"<<'\t'<<"department"<<'\t'<<"cases_seen"<<'\t'<<"cases_seen_new"<<'\t'<<"cases_hospital"<<'\t'<<
            "cases_hospital_new"<<'\t'<<"discharged"<<'\t'<<"discharged_new"<<'\t'<<"death_institution"<<'\t'<<"death_institution_new"<<'\t'<<"death_community"<<'\t'<<
            "death_community_new"<<'\t'<<"death_total"<<'\t'<<"death_total_new"<<'\t'<<"cfr_hospital"<<'\t'<<"cfr_total"<<endl;
    ofs2<<"date"<<'\t'<<"department"<<'\t'<<"cases_seen"<<'\t'<<"cases_seen_new"<<'\t'<<"cases_hospital"<<'\t'<<
    "cases_hospital_new"<<'\t'<<"discharged"<<'\t'<<"discharged_new"<<'\t'<<"death_institution"<<'\t'<<"death_institution_new"<<'\t'<<"death_community"<<'\t'<<
    "death_community_new"<<'\t'<<"death_total"<<'\t'<<"death_total_new"<<'\t'<<"cfr_hospital"<<'\t'<<"cfr_total"<<endl;
    while (flst>>fname)
    {
        string path("csv/");
        path+=fname;
        path+="/Table 1.csv";
        string date;
        string temp;
        ifstream ifs(path.c_str());
        if (!ifs)
        {
            cout<<fname<<endl;
            continue;
        }
        int found_date=0;
        int date_beg;
        int day=0;
        int month=0;
        int year=0;
        for (int i=0;i<20;i++)
        {
            getline(ifs,temp);
            vector<int> date_idx;
            date_idx.clear();
            if (temp[0]>='0' && temp[0]<='9')
            {
                int j;
                for (j=0;j<temp.length();j++)
                {
                    if (temp[j]==' ' || temp[j]==',')
                    {
                        date_idx.push_back(j);
                    }
                    if (temp[j]==',') break;
                }
                if (date_idx.size()!=3) continue;
                string day=temp.substr(0,date_idx[0]);
                string month=temp.substr(date_idx[0]+1,date_idx[1]-date_idx[0]-1);
                string year=temp.substr(date_idx[1]+1,date_idx[2]-date_idx[1]-1);

                if (year.size()==4 && day.size()<=2 && year[0]>='0' && year[0]<='9')
                {
                     found_date=1;
                     date=transform_date(day,month,year);
                     break;
                }
            }
        }
        if (found_date==0)
        {
            continue;
        }
        int first=1;
        int found_a=0;
        int found_b=0;
        int found_c=0;
        while(!ifs.eof())
        {
            getline(ifs,temp);
            if (temp.compare(0,7,"Cas Vus")==0)
            {
                found_a=1;
                input_a(0,temp);
                for (int i=1;i<8;i++)
                {
                    getline(ifs,temp);
                    input_a(i,temp);
                }
            }
            if (temp.compare(0,10,"Artibonite")==0 && first==1)
            {
                found_b=1;
                input_b(0,temp);
                for (int i=1;i<11;i++)
                {
                    getline(ifs,temp);
                    input_b(i,temp);
                }
                first=0;
            }
            if (temp.compare(0,10,"Artibonite")==0)
            {
                found_c=1;
                input_c(0,temp);
                for (int i=1;i<11;i++)
                {
                    getline(ifs,temp);
                    input_c(i,temp);
                }
                first=0;
            }
        }
        if (found_a==0 || found_b==0 || found_c==0)
        {
            continue;
        }
        else
        {
            for (int i=0;i<11;i++)
            {
                ofs<<date<<'\t'<<prov[i];
                ofs2<<date<<'\t'<<prov[i];
                for (int j=0;j<14;j++)
                {
                    ofs<<'\t'<<b[i][j];
                    ofs2<<'\t'<<c[i][j];
                }
                ofs<<endl;
                ofs2<<endl;
            }
            //output table 1
            /*
            for (int i=0;i<8;i++)
            {
                ofs<<date;
                for (int j=0;j<4;j++)
                {
                    ofs<<'\t'<<a[i][j];
                }
                ofs<<endl;
            }*/
        }
        ifs.close();
    }
    ofs.close();
    ofs2.close();
    flst.close();
    return 0;
}
