#include <bits/stdc++.h>
using namespace std;


int partition(vector<int> &vec, int low, int high) {
    int pivot = vec[high]; // Choosing last element as pivot
    int i = low - 1; // Index for smaller element

    for (int j = low; j < high; j++) {
        if (vec[j] <= pivot) {
            i++;
            swap(vec[i], vec[j]);
        }
    }
    swap(vec[i + 1], vec[high]); // Place pivot in correct position
    return i + 1;
}


void iterativeQuickSort(vector<int> &vec) {
    int low = 0, high = vec.size() - 1;

    // Stack to store ends of subarray
    stack<pair<int, int>> st;
    st.push({low, high});

    while (!st.empty()) {
        pair<int, int> p = st.top();
        int low = p.first;
        int high = p.second;

        st.pop();

        int pivotIndex = partition(vec, low, high);

        // Push left part if it has elements
        if (pivotIndex - 1 > low)
            st.push({low, pivotIndex - 1});

        // Push right part if it has elements
        if (pivotIndex + 1 < high)
            st.push({pivotIndex + 1, high});
    }
}

int main() {
    vector<int> arr = {10, 7, 8, 9, 1, 5};
    cout << "Original array: ";
    for (int num : arr)
        cout << num << " ";
    cout << endl;

    iterativeQuickSort(arr);

    cout << "Sorted array: ";
    for (int num : arr)
        cout << num << " ";
    cout << endl;

    return 0;
}
