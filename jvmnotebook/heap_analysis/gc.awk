#
# From:
# http://www.javaworld.com/javaworld/jw-01-2002/jw-0111-hotspotgc.html
BEGIN {
  printf("Minor\tMajor\tAlive\tFreed\n")
}
{
  if ( substr(body,1,4) == "[GC " )  {
    # break each input line into 4 pieces in array[]
    split(body,array," ");
    # array[1]="[GC"
    # array[2]="20713K->549K(64768K),"
    # array[3]="0.0086130"
    # array[4]="secs]"
    printf("%s\t0.0\t",array[3])
    # break array[2]="43960K->1749K(64768K)," into 4 pieces in barray[]
    split(array[2],barray,"K")
    # barray[1]="43960"
    # barray[2]="->1749"
    # barray[3]="(64768"
    # barray[4]="),"
    before=barray[1]
    after=substr(barray[2],3)
    reclaim=before-after
    printf("%s\t%s\n",after,reclaim)
  }
  if ( substr(body,1,9) == "[Full GC " )  {
    # break each input line into 4 pieces in array[]
    split(body,array," ");
    # array[1]="[Full"
    # array[2]="GC"
    # array[3]="20713K->549K(64768K),"
    # array[4]="0.0086130"
    # array[5]="secs]"
    printf("0.0\t%s\t",array[4]) 
    # break array[2]="43960K->1749K(64768K)," into 4 pieces in barray[]
    split(array[3],barray,"K")
    # barray[1]="43960"
    # barray[2]="->1749"
    # barray[3]="(64768"
    # barray[4]="),"
    before=barray[1]
    after=substr(barray[2],3)
    reclaim=before-after
    printf("%s\t%s\n",after,reclaim)
  }
  # no idea what this line is so skip it
  next;
}

