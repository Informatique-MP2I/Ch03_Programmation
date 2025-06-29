#include <stdio.h>

/* additionner deux entiers */
int addition(int nbr_1,int nbr_2){
  return nbr_1 + nbr_2;
}

int main(int argc,char **argv){
  printf("%d\n", addition(2,3));
  return 0;
}
