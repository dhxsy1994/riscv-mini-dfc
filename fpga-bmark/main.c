#include <stdbool.h>

#define SIZE 3
#define LIMIT 5

/*
 * For fpga running.
 * Monitor of DFC interrupt.
 */

//task args
int task_argv[2 * SIZE] = { 7, 2, 9, 3 };

int task_state[SIZE] = { 1, 1 };

int task_update_idx[SIZE] = { 4, 5 };
int size = SIZE;

//compute function
int and(int x, int y) { return x & y; }
int sub(int x, int y) { return x - y; }
int add(int x, int y);

//task node array
int (*task[SIZE])(int, int) = { add, sub, add, };

int scheduler(int x) { return task[x](task_argv[x << 1], task_argv[x << 1 | 1]); }

//asm function
void write_table_a(int, int);       //wta(addr, data)
void write_table_d_info(int, int);  //wtd_info(addr, data)
void write_table_d_addr(int, int*); //wtd_addr(addr, data)

//write TableA data generate
int DAG_wta_data(int count, int in_link, int PID) {
   return (count << 24) + (in_link << 16) + PID;
}

void DAG_write_insts(){

    //dfc ready
        int DAG_c = DAG_wta_data(2, 4, 2);
        write_table_a(1, DAG_c);

        write_table_d_info(4, 1);
        write_table_d_addr(4, task_argv + 4);

        write_table_d_info(5, 1);
        write_table_d_addr(5, task_argv + 5);
}

int main(int argc, char** argv)
{
        // start while
        int fail_cnt = 0, schedu_sum = 0;
        while (true) {
                bool get_task = false;
                DAG_write_insts();
                for (int k = 0; k < SIZE; ++k) {
                        if (1 == task_state[k]) {
                                ++schedu_sum; get_task = true; task_state[k] = 2;
                                int x = scheduler(k);
                                task_argv[task_update_idx[k]] = x;
                        }
                }
                task_state[0] = 1; task_state[1] = 1;

                fail_cnt += (false == get_task);
                if (fail_cnt >= LIMIT) break;
        }
        return 0;
}

