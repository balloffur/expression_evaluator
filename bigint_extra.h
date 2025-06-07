#pragma once
#include <utility>
#include "bigint.h"
#include <random>



//Extended binary GCD. Returns a vector of 3 bigint numbes: Besu coefficients and gcd. Does not work around 0, carefull!
std::vector<bigint> egcd(bigint x,bigint y){
    bigint g=1;    
	while(x%2==0 && y%2==0){
        x>>=1;        
		y>>=1;
        g<<=1;    
	}
    bigint u=x;   
	bigint v=y;
    bigint A=1;
	bigint B=0;
    bigint C=0;
	bigint D=1;
    while(u>0){
    while(u%2==0){int i=u.shift_to_odd();
        for(;i>0;i--){        
			if(A%2==0 && B%2==0){
            A>>=1;            
			B>>=1;
        } else {            
			A=(A+y)>>1;
            B=(B-x)>>1;        
		}
        }    }
    while(v%2==0){        
		int i=v.shift_to_odd();
        for(;i>0;i--){        
			if(C%2==0 && D%2==0){
            C>>=1;            
			D>>=1;
        } else {            
			C=(C+y)>>1;
            D=(D-x)>>1;        
		}
        }    
	}
    if(u>=v){        
		u-=v;
        A-=C;        
		B-=D;
    }else{
        v-=u;        
		C-=A;
        D-=B;    
	}
    }    
	std::vector<bigint> ans={C,D,g*v};
    return ans;}

bigint modular_inverse(bigint x, bigint y){
 	bigint g=1;    
	while(x%2==0 && y%2==0){
		return -1;
        x>>=1;        
		y>>=1;
        g<<=1;    
	}
    bigint u=x;   
	bigint v=y;
    bigint A=1;
	bigint B=0;
    bigint C=0;
	bigint D=1;
    while(u>0){
    while(u%2==0){int i=u.shift_to_odd();
        for(;i>0;i--){        
			if(A%2==0 && B%2==0){
            A>>=1;            
			B>>=1;
        } else {            
			A=(A+y)>>1;
            B=(B-x)>>1;        
		}
        }    }
    while(v%2==0){        
		int i=v.shift_to_odd();
        for(;i>0;i--){        
			if(C%2==0 && D%2==0){
            C>>=1;            
			D>>=1;
        } else {            
			C=(C+y)>>1;
            D=(D-x)>>1;        
		}
        }    
	}
    if(u>=v){        
		u-=v;
        A-=C;        
		B-=D;
    }else{
        v-=u;        
		C-=A;
        D-=B;    
	}
    }
	if(v!=1){return -1;}
    return (C.isNegative()?C+y:C);
}


//randomness
std::random_device dev;
std::mt19937 rng(dev());
std::uniform_int_distribution<std::mt19937::result_type> rand10e9(0,1000000000); // random int <10^9
std::uniform_int_distribution<std::mt19937::result_type> random_stop(0,100); // 

static int cur_random=0;

bigint random_bigint(){
bigint ans;
ans.digits.push_back(rand10e9(rng));
while(random_stop(rng)<90){
ans.digits.push_back(rand10e9(rng));
}
ans.trim();
return ans;
}

bigint random_bigint(int length){
bigint ans;
for(int i=0;i<length-1;i++){
	ans.digits.push_back(rand10e9(rng));
}

return ans;
}




//primes and factors	
std::vector<int> primes={2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,863,877,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997};
static int prime_bound=1000;

//takes ~6ms, all primes under 1000000
void sieve_mil(){
   if(prime_bound>=1000000){
   return;
   }
   prime_bound=1000000;
   primes.reserve(80000);
   int hub_size=10000;
    char temp[hub_size]={};
    for(int i=1;i<25;i++){
      int start=1000%primes[i];
      if(start==0){start=1005;} else {
         start=1000+primes[i]-start;
         if(start%2==0){start+=primes[i];}
      }
      while (start<hub_size)
      {
         temp[start]=1;
         start+=2*primes[i];
      }
    }
    for(int i=1001;i<hub_size;i+=2){
      if(temp[i]==0){primes.push_back(i);}
    }
    for(int i=1;i<1000000/hub_size;i++){
      char temp_o[hub_size]={};
      for(int j=1;primes[j]*primes[j]<hub_size*(i+1);j++){
         int start=hub_size*i%primes[j];
         if(start!=0){
            start=primes[j]-start;
         }
         if(start%2==0){start+=primes[j];}
         while (start<hub_size)
         {
            temp_o[start]=1;
            start+=2*primes[j];
         }
      }
      for(int j=1;j<hub_size;j+=2){
         if(temp_o[j]==0){
            primes.push_back(hub_size*i+j);
         }
      }
    }
}


void sieve_int(){
   prime_bound=2147483646;
   primes.reserve(105151707);
   sieve_mil();
   char temp[48576]={};
   for(int j=1;j<172;j++){
      int start=primes[j]-1000000%primes[j];
      if(start%2==0){start+=primes[j];}
      while(start<48576){
         temp[start]=1;
         start+=2*primes[j];
      }
   }
   for(int i=1;i<48576;i+=2){
      if(temp[i]==0){
         primes.push_back(1000000+i);
      }
   }
   //power of 2
   long long hub_size=16384;

   for(long long i=0;i<2147483648/hub_size;i++){
      char temp[hub_size]={};
      long long cur_r_bound=1048576+(i-1)*hub_size;
      long long cur_l_bound=1048576+i*hub_size;
      int prime_bound=sqrt(cur_r_bound);
      int j=1;
      while(primes[j]<=prime_bound){
         int start=primes[j]-cur_l_bound%primes[j];
         if(start%2==0){start+=primes[j];}
         while(start<hub_size){
            temp[start]=1;
            start+=2*primes[j];
         }
         ++j;
      }
      for(int i=1;i<hub_size;i+=2){
         if(temp[i]==0){primes.push_back(cur_l_bound+i);}
      }
   }
}

//counts how many primes are less than n. binary search euclid
int prime_count(int n){

if(n==2){return 1;}
if(n<2 || n>=prime_bound){
	return -1;
} else {
	int left=0;
	int right=primes.size()-1;
	int middle=(right+left)/2;
	while(right-left>1){
		if(primes[middle]==n){return middle+1;}
		else if(primes[middle]>n){
			right=middle;
			middle=(right+left)/2;
		} else {
			left=middle;
			middle=(right+left)/2;
		}
	}
	return right;
}
}


int32_t pow_mod(int64_t n, int32_t power, int64_t m) {
    int64_t result = 1;

    while (power) {
        if (power & 1) result = (result * n) % m;

        n = (n * n) % m;

        power >>= 1;
    }

    return (int32_t) result;
}

int32_t mul_mod(int32_t a,int32_t b,int32_t m){
	return (int32_t) (((int64_t) a * b) % m);
}
//Тест Миллера-Раббина для int, детерминированный. Используем a= 2, 3, 5. Исключаем 4 псевдопростых из диапазона (1,2^31-1)
bool MillerRabbin(int32_t n) {
    if ((n & 1) == 0) return n == 2;
    if (n < 9) return n > 1; // 3, 5 и 7.

    int32_t s = __builtin_ctz(n - 1);
    int32_t t = (n - 1) >> s;

    int32_t primes[3] = {2, 3, 5};

    for (int32_t a : primes) {
        int32_t x = pow_mod(a, t, n);

        if (x == 1) continue;

        for (int i = 1; x != n - 1; i++) {
            if (i == s) return false;

            x = mul_mod(x, x, n);

            if (x == 1) return false;
        }
    }

	//Псевдопростые для a =2,3,5
    switch (n) {
        case 25326001:
        case 161304001:
        case 960946321:
        case 1157839381:
            return false;

        default:
            return true;
    }
}

//MillerRabbin for bigint
bool MillerRabbin(const bigint& n,int rounds){
bigint n1=n-1;
if(n.even()){return false;}
if(n.digits.size()==1){return MillerRabbin(n.digits[0]);}
bigint t=n1;
long long s=0;
s=t.shift_to_odd();
next:
while(rounds){
	--rounds;
	bigint a=pow_mod(random_bigint(n.digits.size()-1)+1,t,n);
	if(a==1 || a==n1){continue;}
	for(int i=0;i<s-1;i++){
		a=a*a%n;
		if(a==1){return false;}
		if(a==n1){goto next;}
	}
	return false;
}
return true;
}

//MillerRabbin for bigint, default numbers preset of rounds
bool MillerRabbin(const bigint& n){
	if(n==2){return true;}
	if(n.even()){return false;}
	if(n.digits.size()==1){return MillerRabbin(n.digits[0]);}
	std::vector<bigint> preset;
	preset.push_back(bigint(2));
	preset.push_back(bigint(3));
	preset.push_back(bigint(5));
	preset.push_back(n/2);
	preset.push_back(n>>2);
	for(int i=0;i<5;i++){preset.push_back(random_bigint(n.digits.size()-1)+1);}
	int rounds=preset.size();
	bigint n1=n-1;
	bigint t=n1;
	long long s=0;
	s=t.shift_to_odd();
	next:
	while(rounds){
		--rounds;
		bigint a=pow_mod(preset[rounds],t,n);
		if(a==1 || a==n1){continue;}
		for(int i=0;i<s-1;i++){
			a=a*a%n;
			if(a==1){return false;}
			if(a==n1){goto next;}
		}
		return false;
	}
	return true;
	}


bool test_if_prime(bigint a){
	int small_primes[15]={2,3,5,7,11,13,17,19,23,29,31,37,41,43,47};
	
	for(int i=0;i<15;i++){
		if(a%small_primes[i]==0){
			if(a==small_primes[i]){return true;}
			else {return false;}
		}
	}
	return MillerRabbin(a);
}


bigint random_prime_bigint(){
	sieve_mil();
	bigint ans=random_bigint()*2310+primes[rand10e9(rng)%343+5];
	while(!MillerRabbin(ans)){
		ans=random_bigint()*2310+primes[rand10e9(rng)%343+5];
	}
	return ans;
}

bigint random_prime_bigint(int n){
	bigint ans=random_bigint(n)*2310+primes[rand10e9(rng)%343+5];
	while(!test_if_prime(ans)){
		ans=random_bigint(n)*2310+primes[rand10e9(rng)%343+5];
	}
	return ans;
}

struct factorisation{
	std::vector<std::pair<bigint,long long>> list;
	bigint residue;
	bool unfinished=false;

	void print(){
		if(list.size()==0){
			std::cout<<"1";
		} else {
			std::cout<<list[0].first;
			if(list[0].second>1){
				std::cout<<" ^ "<<list[0].second;
			}
			for(int i=1;i<list.size();i++){
				std::cout<<" * "<<list[i].first;
				if(list[i].second>1){
					std::cout<<"^"<<list[i].second;
				}
			}
			if(unfinished){std::cout<<" * "<<residue<<" (not prime)";}
		}
		std::cout<<"\n";
	}	

};



void pollard_rho(bigint a,factorisation& where){
	if(MillerRabbin(a)){
		where.list.push_back(std::make_pair(a,1));
	} else {
		int tries=0;
		while(tries<10){
		again:
		tries++;
		int stage=0;
		bigint x=2;
		bigint y=2;
		bigint d;
		while(stage<10){
			x=(x*x+3)%a;
			y=(y*y+3)%a;
			y=(y*y+3)%a;
			if(x>y){
				d=x-y;
			} else {
				d=y-x;
			}
			d=gcd(d,a);
			if(d==a){goto again;}
			if(d>1){
				pollard_rho(d,where);
				pollard_rho(a/d,where);
			}
			stage++;
		}
		}
	}
}

//simple factorisation using existing primes
factorisation factorise(bigint a){
	if(prime_bound<1000000){sieve_mil();}
	factorisation ans;
	if(a==1){
		return ans;
	}
	if(test_if_prime(a)){
		ans.list.push_back(std::make_pair(a,1));
		return ans;
	}
	for(int i=0;i<primes.size()&&i<78498;i++){
		if(a%primes[i]==0){
			long long power=0;
			while(a%primes[i]==0){
				power++;
				a/=primes[i];
			}
			ans.list.push_back(std::make_pair(bigint(primes[i]),power));
			if(a==1){return ans;}
		}
	}
	if(MillerRabbin(a)){
		ans.list.push_back(std::make_pair(a,1));
		return ans;
	}

	if(a!=1){pollard_rho(a,ans);}
	return ans;
}

// Тест простоты Ферма
bool Fermat(const bigint& n,int tries){
for(;tries;--tries){
	if(pow_mod((n-rand10e9(rng)*rand10e9(rng)-1),n-1,n)!=1) return false;
}
return true;
}






// Combinatorics
namespace {
	bigint factorial(int n){
		if(n<0){return bigint(-1);}
		bigint ans=1;
		for(int i=1;i<=n;i++){
			ans*=i;
		}
		return ans;
	}
	//i
	bigint factorial(bigint n){
		if(n.isZero()){return 1;}
		if(n>1000000000){return -1;}
		if(n<0){return bigint(-1);}
		int m=n.digits[0];
		bigint ans=1;
		for(int i=1;i<m;i++){
			ans*=i;
		}
		return ans;
	}

	//binomial
	bigint binomial(int n,int k){
		if(n-k<k){k=n-k;}
		bigint ans=1;
		for(int i=1;i<=k;i++){
			ans*=(n-i+1);
			ans/=i;
		}
		return ans;
	}
	
	//partial permutations
	bigint partperm(int n,int k){
		bigint ans=1;
		for(int i=k+1;i<=n;i++){
			ans*=i;
		}
		return ans;
	}
	
	//fibonacci
	bigint fibonacci(int n){
		if(n<0){return bigint(-1);}
		int small_fibs[]={0,1,1,2,3,5,8,13,21,34,55};
		if(n<=10){
			return bigint(small_fibs[n]);
		}
		std::vector<char> digits;
		while(n){
			digits.push_back(n%2);
			n/=2;
		}
		bigint a=1;
		bigint b=1;
		bigint c=0;
		for(int i=digits.size()-2;i>=0;i--){
			a*=a;
			b*=b;
			c*=c;
			a+=b;
			c+=b;
			b=a-c;
			if(digits[i]){
				c=b;
				b=a;
				a+=c;
			}
		}
		return b;
	}
}

