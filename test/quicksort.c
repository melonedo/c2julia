#include <stdio.h>
#include <stdlib.h>

int partition(int *arr, int begin, int end) {
  int pivot = arr[begin];
  int i = begin;
  int j = end - 1;
  while (i < j) {
    while (i < j && arr[j] >= pivot)
      j--;
    arr[i] = arr[j];
    while (i < j && arr[i] <= pivot)
      i++;
    arr[j] = arr[i];
  }
  arr[i] = pivot;
  return i;
}

void quicksort(int *arr, int begin, int end) {
  if (begin == end)
    return;
  int mid = partition(arr, begin, end);
  quicksort(arr, begin, mid);
  quicksort(arr, mid + 1, end);
}

int main() {
  int len;
  scanf("%d", &len);
  int *nums = malloc(len * sizeof(int));
  for (int i = 0; i < len; i++) {
    scanf("%d", &nums[i]);
  }

  quicksort(nums, 0, len);

  for (int i = 0; i < len; i++) {
    printf("%d ", nums[i]);
  }
  putchar('\n');

  free(nums);
  return 0;
}