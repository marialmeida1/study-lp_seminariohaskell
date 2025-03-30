public class studyCase2 {

    /**
	 * Algoritmo de ordenacao Quicksort.
     * @param int[] array vetor a ser ordenado
     * @param int esq inicio do array a ser ordenado
     * @param int dir fim do array a ser ordenado
	 */
    private void quicksort(int[] array, int esq, int dir) {
        int i = esq, j = dir;
        int pivo = array[(dir+esq)/2];
        while (i <= j) {
            while (array[i] < pivo) i++;
            while (array[j] > pivo) j--;
            if (i <= j) {
                swap(array, i, j);
                i++;
                j--;
            }
        }
        if (esq < j)  quicksort(array,esq, j);
        if (i < dir)  quicksort(array,i, dir);
    }
    
}
