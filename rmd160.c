/********************************************************************\
 *
 *      FILE:     rmd160.c
 *
 *      CONTENTS: A sample C-implementation of the RIPEMD-160
 *                hash-function.
 *      TARGET:   any computer with an ANSI C compiler
 *
 *      AUTHOR:   Antoon Bosselaers, ESAT-COSIC
 *      DATE:     1 March 1996
 *      VERSION:  1.0
 *
 *      Copyright (c) Katholieke Universiteit Leuven
 *      1996, All Rights Reserved
 *
\********************************************************************/

/*  header files */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "rmd160.h"      

/********************************************************************/

void MDinit(dword *MDbuf)
{
   MDbuf[0] = 0x67452301UL;
   MDbuf[1] = 0xefcdab89UL;
   MDbuf[2] = 0x98badcfeUL;
   MDbuf[3] = 0x10325476UL;
   MDbuf[4] = 0xc3d2e1f0UL;

   return;
}

void dumpboxes(const char *message, dword a, dword b, dword c, dword d, dword e)
{
    unsigned i,j;
    dword mdbuf[5] = {a,b,c,d,e};
    printf("%s", message);
    for (i = 0; i < 5; ++i)
    {
        for (j = 0; j < 4; ++j)
        {
            printf("%02x ", *(((byte*)&mdbuf[i]) + j));
        }
        printf("\n");
    }
}

/********************************************************************/

void compress(dword *MDbuf, dword *X)
{
   dword aa = MDbuf[0],  bb = MDbuf[1],  cc = MDbuf[2],
         dd = MDbuf[3],  ee = MDbuf[4];
   dword aaa = MDbuf[0], bbb = MDbuf[1], ccc = MDbuf[2],
         ddd = MDbuf[3], eee = MDbuf[4];

   /* round 1 */
   dumpboxes("before\n", aa,bb,cc,dd,ee);
   printf("after fun: %x\n",   F((bb), (cc), (dd)));
   printf("X[0]: %x %x %x %x\n",
     *(((byte*)&X[0])+0),
     *(((byte*)&X[0])+1),
     *(((byte*)&X[0])+2),
     *(((byte*)&X[0])+3)
   );
   FF(aa, bb, cc, dd, ee, X[ 0], 11);
   dumpboxes("FF 0\n", aa,bb,cc,dd,ee);
   printf("x0: %02x\n", X[0]);
   FF(ee, aa, bb, cc, dd, X[ 1], 14);
   dumpboxes("FF 1\n", ee, aa, bb, cc, dd);
   FF(dd, ee, aa, bb, cc, X[ 2], 15);
   dumpboxes("FF 2\n", dd, ee, aa, bb, cc);
   FF(cc, dd, ee, aa, bb, X[ 3], 12);
   dumpboxes("FF 3\n", cc, dd, ee, aa, bb);
   FF(bb, cc, dd, ee, aa, X[ 4],  5);
   dumpboxes("FF 4\n", bb, cc, dd, ee, aa);
   FF(aa, bb, cc, dd, ee, X[ 5],  8);
   dumpboxes("FF 5\n", aa, bb, cc, dd, ee);
   FF(ee, aa, bb, cc, dd, X[ 6],  7);
   dumpboxes("FF 6\n", ee, aa, bb, cc, dd);
   FF(dd, ee, aa, bb, cc, X[ 7],  9);
   dumpboxes("FF 7\n", dd, ee, aa, bb, cc);
   FF(cc, dd, ee, aa, bb, X[ 8], 11);
   dumpboxes("FF 8\n", cc, dd, ee, aa, bb);
   FF(bb, cc, dd, ee, aa, X[ 9], 13);
   dumpboxes("FF 9\n", bb, cc, dd, ee, aa);
   FF(aa, bb, cc, dd, ee, X[10], 14);
   dumpboxes("FF 10\n", aa, bb, cc, dd, ee);
   FF(ee, aa, bb, cc, dd, X[11], 15);
   dumpboxes("FF 11\n", ee, aa, bb, cc, dd);
   FF(dd, ee, aa, bb, cc, X[12],  6);
   dumpboxes("FF 12\n", dd, ee, aa, bb, cc);
   FF(cc, dd, ee, aa, bb, X[13],  7);
   dumpboxes("FF 13\n", cc, dd, ee, aa, bb);
   FF(bb, cc, dd, ee, aa, X[14],  9);
   dumpboxes("FF 14\n", bb, cc, dd, ee, aa);
   FF(aa, bb, cc, dd, ee, X[15],  8);
   dumpboxes("FF 15\n", aa, bb, cc, dd, ee);
                             
   /* round 2 */
   GG(ee, aa, bb, cc, dd, X[ 7],  7);
   dumpboxes("GG 1\n", ee, aa, bb, cc, dd);
   GG(dd, ee, aa, bb, cc, X[ 4],  6);
   dumpboxes("GG 2\n", dd, ee, aa, bb, cc);
   GG(cc, dd, ee, aa, bb, X[13],  8);
   dumpboxes("GG 3\n", cc, dd, ee, aa, bb);
   GG(bb, cc, dd, ee, aa, X[ 1], 13);
   dumpboxes("GG 4\n", bb, cc, dd, ee, aa);
   GG(aa, bb, cc, dd, ee, X[10], 11);
   dumpboxes("GG 5\n", aa, bb, cc, dd, ee);
   GG(ee, aa, bb, cc, dd, X[ 6],  9);
   dumpboxes("GG 6\n", ee, aa, bb, cc, dd);
   GG(dd, ee, aa, bb, cc, X[15],  7);
   dumpboxes("GG 7\n", dd, ee, aa, bb, cc);
   GG(cc, dd, ee, aa, bb, X[ 3], 15);
   dumpboxes("GG 8\n", cc, dd, ee, aa, bb);
   GG(bb, cc, dd, ee, aa, X[12],  7);
   dumpboxes("GG 9\n", bb, cc, dd, ee, aa);
   GG(aa, bb, cc, dd, ee, X[ 0], 12);
   dumpboxes("GG 10\n", aa, bb, cc, dd, ee);
   GG(ee, aa, bb, cc, dd, X[ 9], 15);
   dumpboxes("GG 11\n", ee, aa, bb, cc, dd);
   GG(dd, ee, aa, bb, cc, X[ 5],  9);
   dumpboxes("GG 12\n", dd, ee, aa, bb, cc);
   GG(cc, dd, ee, aa, bb, X[ 2], 11);
   dumpboxes("GG 13\n", cc, dd, ee, aa, bb);
   GG(bb, cc, dd, ee, aa, X[14],  7);
   dumpboxes("GG 14\n", bb, cc, dd, ee, aa);
   GG(aa, bb, cc, dd, ee, X[11], 13);
   dumpboxes("GG 15\n", aa, bb, cc, dd, ee);
   GG(ee, aa, bb, cc, dd, X[ 8], 12);
   dumpboxes("GG 16\n", ee, aa, bb, cc, dd);

   /* round 3 */
   HH(dd, ee, aa, bb, cc, X[ 3], 11);
   dumpboxes("HH\n", dd, ee, aa, bb, cc);
   HH(cc, dd, ee, aa, bb, X[10], 13);
   dumpboxes("HH\n", cc, dd, ee, aa, bb);
   HH(bb, cc, dd, ee, aa, X[14],  6);
   dumpboxes("HH\n", bb, cc, dd, ee, aa);
   HH(aa, bb, cc, dd, ee, X[ 4],  7);
   dumpboxes("HH\n", aa, bb, cc, dd, ee);
   HH(ee, aa, bb, cc, dd, X[ 9], 14);
   dumpboxes("HH\n", ee, aa, bb, cc, dd);
   HH(dd, ee, aa, bb, cc, X[15],  9);
   dumpboxes("HH\n", dd, ee, aa, bb, cc);
   HH(cc, dd, ee, aa, bb, X[ 8], 13);
   dumpboxes("HH\n", cc, dd, ee, aa, bb);
   HH(bb, cc, dd, ee, aa, X[ 1], 15);
   dumpboxes("HH\n", bb, cc, dd, ee, aa);
   HH(aa, bb, cc, dd, ee, X[ 2], 14);
   dumpboxes("HH\n", aa, bb, cc, dd, ee);
   HH(ee, aa, bb, cc, dd, X[ 7],  8);
   dumpboxes("HH\n", ee, aa, bb, cc, dd);
   HH(dd, ee, aa, bb, cc, X[ 0], 13);
   dumpboxes("HH\n", dd, ee, aa, bb, cc);
   HH(cc, dd, ee, aa, bb, X[ 6],  6);
   dumpboxes("HH\n", cc, dd, ee, aa, bb);
   HH(bb, cc, dd, ee, aa, X[13],  5);
   dumpboxes("HH\n", bb, cc, dd, ee, aa);
   HH(aa, bb, cc, dd, ee, X[11], 12);
   dumpboxes("HH\n", aa, bb, cc, dd, ee);
   HH(ee, aa, bb, cc, dd, X[ 5],  7);
   dumpboxes("HH\n", ee, aa, bb, cc, dd);
   HH(dd, ee, aa, bb, cc, X[12],  5);
   dumpboxes("HH\n", dd, ee, aa, bb, cc);

   /* round 4 */
   II(cc, dd, ee, aa, bb, X[ 1], 11);
   dumpboxes("II\n", cc, dd, ee, aa, bb);
   II(bb, cc, dd, ee, aa, X[ 9], 12);
   dumpboxes("II\n", bb, cc, dd, ee, aa);
   II(aa, bb, cc, dd, ee, X[11], 14);
   dumpboxes("II\n", aa, bb, cc, dd, ee);
   II(ee, aa, bb, cc, dd, X[10], 15);
   dumpboxes("II\n", ee, aa, bb, cc, dd);
   II(dd, ee, aa, bb, cc, X[ 0], 14);
   dumpboxes("II\n", dd, ee, aa, bb, cc);
   II(cc, dd, ee, aa, bb, X[ 8], 15);
   dumpboxes("II\n", cc, dd, ee, aa, bb);
   II(bb, cc, dd, ee, aa, X[12],  9);
   dumpboxes("II\n", bb, cc, dd, ee, aa);
   II(aa, bb, cc, dd, ee, X[ 4],  8);
   dumpboxes("II\n", aa, bb, cc, dd, ee);
   II(ee, aa, bb, cc, dd, X[13],  9);
   dumpboxes("II\n", ee, aa, bb, cc, dd);
   II(dd, ee, aa, bb, cc, X[ 3], 14);
   dumpboxes("II\n", dd, ee, aa, bb, cc);
   II(cc, dd, ee, aa, bb, X[ 7],  5);
   dumpboxes("II\n", cc, dd, ee, aa, bb);
   II(bb, cc, dd, ee, aa, X[15],  6);
   dumpboxes("II\n", bb, cc, dd, ee, aa);
   II(aa, bb, cc, dd, ee, X[14],  8);
   dumpboxes("II\n", aa, bb, cc, dd, ee);
   II(ee, aa, bb, cc, dd, X[ 5],  6);
   dumpboxes("II\n", ee, aa, bb, cc, dd);
   II(dd, ee, aa, bb, cc, X[ 6],  5);
   dumpboxes("II\n", dd, ee, aa, bb, cc);
   II(cc, dd, ee, aa, bb, X[ 2], 12);
   dumpboxes("II\n", cc, dd, ee, aa, bb);

   /* round 5 */
   JJ(bb, cc, dd, ee, aa, X[ 4],  9);
   dumpboxes("JJ\n", bb, cc, dd, ee, aa);
   JJ(aa, bb, cc, dd, ee, X[ 0], 15);
   dumpboxes("JJ\n", aa, bb, cc, dd, ee);
   JJ(ee, aa, bb, cc, dd, X[ 5],  5);
   dumpboxes("JJ\n", ee, aa, bb, cc, dd);
   JJ(dd, ee, aa, bb, cc, X[ 9], 11);
   dumpboxes("JJ\n", dd, ee, aa, bb, cc);
   JJ(cc, dd, ee, aa, bb, X[ 7],  6);
   dumpboxes("JJ\n", cc, dd, ee, aa, bb);
   JJ(bb, cc, dd, ee, aa, X[12],  8);
   dumpboxes("JJ\n", bb, cc, dd, ee, aa);
   JJ(aa, bb, cc, dd, ee, X[ 2], 13);
   dumpboxes("JJ\n", aa, bb, cc, dd, ee);
   JJ(ee, aa, bb, cc, dd, X[10], 12);
   dumpboxes("JJ\n", ee, aa, bb, cc, dd);
   JJ(dd, ee, aa, bb, cc, X[14],  5);
   dumpboxes("JJ\n", dd, ee, aa, bb, cc);
   JJ(cc, dd, ee, aa, bb, X[ 1], 12);
   dumpboxes("JJ\n", cc, dd, ee, aa, bb);
   JJ(bb, cc, dd, ee, aa, X[ 3], 13);
   dumpboxes("JJ\n", bb, cc, dd, ee, aa);
   JJ(aa, bb, cc, dd, ee, X[ 8], 14);
   dumpboxes("JJ\n", aa, bb, cc, dd, ee);
   JJ(ee, aa, bb, cc, dd, X[11], 11);
   dumpboxes("JJ\n", ee, aa, bb, cc, dd);
   JJ(dd, ee, aa, bb, cc, X[ 6],  8);
   dumpboxes("JJ\n", dd, ee, aa, bb, cc);
   JJ(cc, dd, ee, aa, bb, X[15],  5);
   dumpboxes("JJ\n", cc, dd, ee, aa, bb);
   JJ(bb, cc, dd, ee, aa, X[13],  6);
   dumpboxes("JJ\n", bb, cc, dd, ee, aa);

   /* parallel round 1 */
   JJJ(aaa, bbb, ccc, ddd, eee, X[ 5],  8);
   dumpboxes("JJJ\n", aaa, bbb, ccc, ddd, eee);
   JJJ(eee, aaa, bbb, ccc, ddd, X[14],  9);
   dumpboxes("JJJ\n", eee, aaa, bbb, ccc, ddd);
   JJJ(ddd, eee, aaa, bbb, ccc, X[ 7],  9);
   dumpboxes("JJJ\n", ddd, eee, aaa, bbb, ccc);
   JJJ(ccc, ddd, eee, aaa, bbb, X[ 0], 11);
   dumpboxes("JJJ\n", ccc, ddd, eee, aaa, bbb);
   JJJ(bbb, ccc, ddd, eee, aaa, X[ 9], 13);
   dumpboxes("JJJ\n", bbb, ccc, ddd, eee, aaa);
   JJJ(aaa, bbb, ccc, ddd, eee, X[ 2], 15);
   dumpboxes("JJJ\n", aaa, bbb, ccc, ddd, eee);
   JJJ(eee, aaa, bbb, ccc, ddd, X[11], 15);
   dumpboxes("JJJ\n", eee, aaa, bbb, ccc, ddd);
   JJJ(ddd, eee, aaa, bbb, ccc, X[ 4],  5);
   dumpboxes("JJJ\n", ddd, eee, aaa, bbb, ccc);
   JJJ(ccc, ddd, eee, aaa, bbb, X[13],  7);
   dumpboxes("JJJ\n", ccc, ddd, eee, aaa, bbb);
   JJJ(bbb, ccc, ddd, eee, aaa, X[ 6],  7);
   dumpboxes("JJJ\n", bbb, ccc, ddd, eee, aaa);
   JJJ(aaa, bbb, ccc, ddd, eee, X[15],  8);
   dumpboxes("JJJ\n", aaa, bbb, ccc, ddd, eee);
   JJJ(eee, aaa, bbb, ccc, ddd, X[ 8], 11);
   dumpboxes("JJJ\n", eee, aaa, bbb, ccc, ddd);
   JJJ(ddd, eee, aaa, bbb, ccc, X[ 1], 14);
   dumpboxes("JJJ\n", ddd, eee, aaa, bbb, ccc);
   JJJ(ccc, ddd, eee, aaa, bbb, X[10], 14);
   dumpboxes("JJJ\n", ccc, ddd, eee, aaa, bbb);
   JJJ(bbb, ccc, ddd, eee, aaa, X[ 3], 12);
   dumpboxes("JJJ\n", bbb, ccc, ddd, eee, aaa);
   JJJ(aaa, bbb, ccc, ddd, eee, X[12],  6);
   dumpboxes("JJJ\n", aaa, bbb, ccc, ddd, eee);

   /* parallel round 2 */
   III(eee, aaa, bbb, ccc, ddd, X[ 6],  9); 
   dumpboxes("III\n", eee, aaa, bbb, ccc, ddd);
   III(ddd, eee, aaa, bbb, ccc, X[11], 13);
   dumpboxes("III\n", ddd, eee, aaa, bbb, ccc);
   III(ccc, ddd, eee, aaa, bbb, X[ 3], 15);
   dumpboxes("III\n", ccc, ddd, eee, aaa, bbb);
   III(bbb, ccc, ddd, eee, aaa, X[ 7],  7);
   dumpboxes("III\n", bbb, ccc, ddd, eee, aaa);
   III(aaa, bbb, ccc, ddd, eee, X[ 0], 12);
   dumpboxes("III\n", aaa, bbb, ccc, ddd, eee);
   III(eee, aaa, bbb, ccc, ddd, X[13],  8);
   dumpboxes("III\n", eee, aaa, bbb, ccc, ddd);
   III(ddd, eee, aaa, bbb, ccc, X[ 5],  9);
   dumpboxes("III\n", ddd, eee, aaa, bbb, ccc);
   III(ccc, ddd, eee, aaa, bbb, X[10], 11);
   dumpboxes("III\n", ccc, ddd, eee, aaa, bbb);
   III(bbb, ccc, ddd, eee, aaa, X[14],  7);
   dumpboxes("III\n", bbb, ccc, ddd, eee, aaa);
   III(aaa, bbb, ccc, ddd, eee, X[15],  7);
   dumpboxes("III\n", aaa, bbb, ccc, ddd, eee);
   III(eee, aaa, bbb, ccc, ddd, X[ 8], 12);
   dumpboxes("III\n", eee, aaa, bbb, ccc, ddd);
   III(ddd, eee, aaa, bbb, ccc, X[12],  7);
   dumpboxes("III\n", ddd, eee, aaa, bbb, ccc);
   III(ccc, ddd, eee, aaa, bbb, X[ 4],  6);
   dumpboxes("III\n", ccc, ddd, eee, aaa, bbb);
   III(bbb, ccc, ddd, eee, aaa, X[ 9], 15);
   dumpboxes("III\n", bbb, ccc, ddd, eee, aaa);
   III(aaa, bbb, ccc, ddd, eee, X[ 1], 13);
   dumpboxes("III\n", aaa, bbb, ccc, ddd, eee);
   III(eee, aaa, bbb, ccc, ddd, X[ 2], 11);
   dumpboxes("III\n", eee, aaa, bbb, ccc, ddd);

   /* parallel round 3 */
   HHH(ddd, eee, aaa, bbb, ccc, X[15],  9);
   dumpboxes("HHH\n", ddd, eee, aaa, bbb, ccc);
   HHH(ccc, ddd, eee, aaa, bbb, X[ 5],  7);
   dumpboxes("HHH\n", ccc, ddd, eee, aaa, bbb);
   HHH(bbb, ccc, ddd, eee, aaa, X[ 1], 15);
   dumpboxes("HHH\n", bbb, ccc, ddd, eee, aaa);
   HHH(aaa, bbb, ccc, ddd, eee, X[ 3], 11);
   dumpboxes("HHH\n", aaa, bbb, ccc, ddd, eee);
   HHH(eee, aaa, bbb, ccc, ddd, X[ 7],  8);
   dumpboxes("HHH\n", eee, aaa, bbb, ccc, ddd);
   HHH(ddd, eee, aaa, bbb, ccc, X[14],  6);
   dumpboxes("HHH\n", ddd, eee, aaa, bbb, ccc);
   HHH(ccc, ddd, eee, aaa, bbb, X[ 6],  6);
   dumpboxes("HHH\n", ccc, ddd, eee, aaa, bbb);
   HHH(bbb, ccc, ddd, eee, aaa, X[ 9], 14);
   dumpboxes("HHH\n", bbb, ccc, ddd, eee, aaa);
   HHH(aaa, bbb, ccc, ddd, eee, X[11], 12);
   dumpboxes("HHH\n", aaa, bbb, ccc, ddd, eee);
   HHH(eee, aaa, bbb, ccc, ddd, X[ 8], 13);
   dumpboxes("HHH\n", eee, aaa, bbb, ccc, ddd);
   HHH(ddd, eee, aaa, bbb, ccc, X[12],  5);
   dumpboxes("HHH\n", ddd, eee, aaa, bbb, ccc);
   HHH(ccc, ddd, eee, aaa, bbb, X[ 2], 14);
   dumpboxes("HHH\n", ccc, ddd, eee, aaa, bbb);
   HHH(bbb, ccc, ddd, eee, aaa, X[10], 13);
   dumpboxes("HHH\n", bbb, ccc, ddd, eee, aaa);
   HHH(aaa, bbb, ccc, ddd, eee, X[ 0], 13);
   dumpboxes("HHH\n", aaa, bbb, ccc, ddd, eee);
   HHH(eee, aaa, bbb, ccc, ddd, X[ 4],  7);
   dumpboxes("HHH\n", eee, aaa, bbb, ccc, ddd);
   HHH(ddd, eee, aaa, bbb, ccc, X[13],  5);
   dumpboxes("HHH\n", ddd, eee, aaa, bbb, ccc);

   /* parallel round 4 */   
   GGG(ccc, ddd, eee, aaa, bbb, X[ 8], 15);
   dumpboxes("GGG\n", ccc, ddd, eee, aaa, bbb);
   GGG(bbb, ccc, ddd, eee, aaa, X[ 6],  5);
   dumpboxes("GGG\n", bbb, ccc, ddd, eee, aaa);
   GGG(aaa, bbb, ccc, ddd, eee, X[ 4],  8);
   dumpboxes("GGG\n", aaa, bbb, ccc, ddd, eee);
   GGG(eee, aaa, bbb, ccc, ddd, X[ 1], 11);
   dumpboxes("GGG\n", eee, aaa, bbb, ccc, ddd);
   GGG(ddd, eee, aaa, bbb, ccc, X[ 3], 14);
   dumpboxes("GGG\n", ddd, eee, aaa, bbb, ccc);
   GGG(ccc, ddd, eee, aaa, bbb, X[11], 14);
   dumpboxes("GGG\n", ccc, ddd, eee, aaa, bbb);
   GGG(bbb, ccc, ddd, eee, aaa, X[15],  6);
   dumpboxes("GGG\n", bbb, ccc, ddd, eee, aaa);
   GGG(aaa, bbb, ccc, ddd, eee, X[ 0], 14);
   dumpboxes("GGG\n", aaa, bbb, ccc, ddd, eee);
   GGG(eee, aaa, bbb, ccc, ddd, X[ 5],  6);
   dumpboxes("GGG\n", eee, aaa, bbb, ccc, ddd);
   GGG(ddd, eee, aaa, bbb, ccc, X[12],  9);
   dumpboxes("GGG\n", ddd, eee, aaa, bbb, ccc);
   GGG(ccc, ddd, eee, aaa, bbb, X[ 2], 12);
   dumpboxes("GGG\n", ccc, ddd, eee, aaa, bbb);
   GGG(bbb, ccc, ddd, eee, aaa, X[13],  9);
   dumpboxes("GGG\n", bbb, ccc, ddd, eee, aaa);
   GGG(aaa, bbb, ccc, ddd, eee, X[ 9], 12);
   dumpboxes("GGG\n", aaa, bbb, ccc, ddd, eee);
   GGG(eee, aaa, bbb, ccc, ddd, X[ 7],  5);
   dumpboxes("GGG\n", eee, aaa, bbb, ccc, ddd);
   GGG(ddd, eee, aaa, bbb, ccc, X[10], 15);
   dumpboxes("GGG\n", ddd, eee, aaa, bbb, ccc);
   GGG(ccc, ddd, eee, aaa, bbb, X[14],  8);
   dumpboxes("GGG\n", ccc, ddd, eee, aaa, bbb);

   /* parallel round 5 */
   FFF(bbb, ccc, ddd, eee, aaa, X[12] ,  8);
   dumpboxes("FFF\n", bbb, ccc, ddd, eee, aaa);
   FFF(aaa, bbb, ccc, ddd, eee, X[15] ,  5);
   dumpboxes("FFF\n", aaa, bbb, ccc, ddd, eee);
   FFF(eee, aaa, bbb, ccc, ddd, X[10] , 12);
   dumpboxes("FFF\n", eee, aaa, bbb, ccc, ddd);
   FFF(ddd, eee, aaa, bbb, ccc, X[ 4] ,  9);
   dumpboxes("FFF\n", ddd, eee, aaa, bbb, ccc);
   FFF(ccc, ddd, eee, aaa, bbb, X[ 1] , 12);
   dumpboxes("FFF\n", ccc, ddd, eee, aaa, bbb);
   FFF(bbb, ccc, ddd, eee, aaa, X[ 5] ,  5);
   dumpboxes("FFF\n", bbb, ccc, ddd, eee, aaa);
   FFF(aaa, bbb, ccc, ddd, eee, X[ 8] , 14);
   dumpboxes("FFF\n", aaa, bbb, ccc, ddd, eee);
   FFF(eee, aaa, bbb, ccc, ddd, X[ 7] ,  6);
   dumpboxes("FFF\n", eee, aaa, bbb, ccc, ddd);
   FFF(ddd, eee, aaa, bbb, ccc, X[ 6] ,  8);
   dumpboxes("FFF\n", ddd, eee, aaa, bbb, ccc);
   FFF(ccc, ddd, eee, aaa, bbb, X[ 2] , 13);
   dumpboxes("FFF\n", ccc, ddd, eee, aaa, bbb);
   FFF(bbb, ccc, ddd, eee, aaa, X[13] ,  6);
   dumpboxes("FFF\n", bbb, ccc, ddd, eee, aaa);
   FFF(aaa, bbb, ccc, ddd, eee, X[14] ,  5);
   dumpboxes("FFF\n", aaa, bbb, ccc, ddd, eee);
   FFF(eee, aaa, bbb, ccc, ddd, X[ 0] , 15);
   dumpboxes("FFF\n", eee, aaa, bbb, ccc, ddd);
   FFF(ddd, eee, aaa, bbb, ccc, X[ 3] , 13);
   dumpboxes("FFF\n", ddd, eee, aaa, bbb, ccc);
   FFF(ccc, ddd, eee, aaa, bbb, X[ 9] , 11);
   dumpboxes("FFF\n", ccc, ddd, eee, aaa, bbb);
   FFF(bbb, ccc, ddd, eee, aaa, X[11] , 11);
   dumpboxes("FFF\n", bbb, ccc, ddd, eee, aaa);

   dumpboxes("orig\n", MDbuf[0], MDbuf[1], MDbuf[2], MDbuf[3], MDbuf[4]);
   dumpboxes("left\n", aa, bb, cc, dd, ee);
   dumpboxes("right\n", aaa, bbb, ccc, ddd, eee);
   /* combine results */
   ddd += cc + MDbuf[1];               /* final result for MDbuf[0] */
   MDbuf[1] = MDbuf[2] + dd + eee;
   MDbuf[2] = MDbuf[3] + ee + aaa;
   MDbuf[3] = MDbuf[4] + aa + bbb;
   MDbuf[4] = MDbuf[0] + bb + ccc;
   MDbuf[0] = ddd;

   return;
}

/********************************************************************/

void MDfinish(dword *MDbuf, byte *strptr, dword lswlen, dword mswlen)
{
   unsigned int i;                                 /* counter       */
   dword        X[16];                             /* message words */

   memset(X, 0, 16*sizeof(dword));

   /* put bytes from strptr into X */
   for (i=0; i<(lswlen&63); i++) {
      /* byte i goes into word X[i div 4] at pos.  8*(i mod 4)  */
      X[i>>2] ^= (dword) *strptr++ << (8 * (i&3));
   }

   /* append the bit m_n == 1 */
   X[(lswlen>>2)&15] ^= (dword)1 << (8*(lswlen&3) + 7);

   if ((lswlen & 63) > 55) {
      /* length goes to next block */
      compress(MDbuf, X);
      memset(X, 0, 16*sizeof(dword));
   }

   /* append length in bits*/
   X[14] = lswlen << 3;
   X[15] = (lswlen >> 29) | (mswlen << 3);
   compress(MDbuf, X);

   return;
}

/************************ end of file rmd160.c **********************/

