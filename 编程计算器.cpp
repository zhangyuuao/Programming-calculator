#include<cstdio>
#include<cstdlib>
#include<iostream>
#include<cstring>
#include<cmath>
#define TRUE        1
#define FALSE       0
#define OK          1
#define ERROR       0
#define INFEASIBLE -1
#define Overflow   -2
#define LIST_INIT_SIZE  100
#define LISTINCREMENT    10
#define STACK_INIT_SIZE 100
#define STACKINCREMENT   10
#define MAXSTRLEN       255
#define eps            1E-5

using namespace  std ;

typedef int Status ;
typedef int ElemType ;
typedef ElemType SElemType ;

//一元多项式的每一项
typedef struct term 
{
    float coef ; //系数
    int expn ; //指数
} term ; 

//顺序表的存储结构的结构体
typedef struct Sqlist
{
	ElemType *elem ; //存储空间基址
	int length ;
	int listsize ;
} Sqlist ; 

//线性链表结点
typedef struct LNode 
{
    term data ; 
    struct LNode *next ; 
} LNode , *LinkList ; 

//多项式的顺序表
typedef struct 
{
	term *elem ;
	int length ;
	int listsize ;
} Sq_p ; 

typedef LinkList polynomail ; //线性表的链式存储结构

//矩阵的结构体
typedef struct{
	string var_name ;
	int row , col ;
	double **matrix ;
} Matrix ;

//变量和它的值
typedef struct{
    string ch ; //变量名
    double var ; //变量的值
}SString ;

//函数和他的表达式
typedef struct{
    string ch ; //函数名
    string expression ; //函数的表达式
}Func ;

//接下来的为链表的各种操作


//创建一元多项式链表p
Status CreatPolynomial(polynomail &p){
	p = (polynomail)malloc(sizeof(LNode)) ;
	p -> next = NULL ;
	return OK ;
}

//渲染输出一元多项式
Status print_p(polynomail p){
	polynomail q = p -> next ;
	int k = 1 ;
	cout << "该多项式为：" ;
	while(q){
		if(q -> data.coef > 0 && k != 1) printf("+") ;
		if(q -> data.coef && q -> data.expn) printf("%.2fx^%d" , q -> data.coef , q -> data.expn) ;
		else if(q -> data.expn == 0 && q -> data.coef) printf("%.2f" , q -> data.coef) ;
		q = q -> next ;
		k++ ;
	}
	printf("\n") ;
	return OK ;
}

//排序一元多项式链表
Status sort_poly(polynomail &p){
	polynomail j , k ;
	polynomail q = p ;
	if(!(q -> next -> next)) return OK ;
	q = q -> next ;
	while(q){
		j = q ;
		k = j -> next ;
		while(k){
			if(j -> data.expn > k -> data.expn) j = k ;
			k = k -> next ;
		}
		if(q != j){
			term tmp ;
			tmp = q -> data ;
			q -> data = j -> data ;
			j -> data = tmp ;
		}
		q = q -> next ;
	}
	return OK ;
}

//销毁多项式链表
Status DestroyPolynomial(polynomail &p){
	polynomail t1 = p ;
	polynomail t2 ;
	while(t1){
		t2 = t1 -> next ;
		free(t1) ;
		t1 = t2 ;
	}
	p = NULL ;
	return OK ;
}

//查看多项式长度
int PolynLength(polynomail p){
	polynomail q = p -> next ;
	int i = 0 ;
	while(q){
		q = q -> next ;
		i++ ;
	}
	return i ;
}

//向链表的尾部增加元素
Status Poly_Insert_tail(polynomail &pa , term e){
	polynomail p = pa ;
	while(p -> next) p = p -> next ;
	polynomail p3 = (polynomail)malloc(sizeof(LNode)) ;
	p3 -> data = e ;
	p -> next = p3 ;
	p3 -> next = NULL ;
	return OK ;
}

//链表多项式加法
Status Poly_add(polynomail &pa , polynomail pb , polynomail &pc){
	polynomail la = pa -> next ;
	polynomail lb = pb -> next ;
	polynomail lc ;
	pc = pa ;
	lc = pa ;
	while(la && lb){
		if(la -> data.expn < lb -> data.expn){
			lc -> next = la ;
			lc = la ;
			la = la -> next ;
		}
		else if(la -> data.expn < lb -> data.expn){
			lc -> next = lb ;
			lc = lb ;
			lb = lb -> next ;
		}
		else{
			la -> data.coef = la -> data.coef + lb -> data.coef ;
			lc -> next = la ;
			lc = la ;
			la = la -> next ;
			lb = lb -> next ;
		}
	}
	lc -> next = la ? la : lb ;
	free(pb) ;
	print_p(pc) ;
	DestroyPolynomial(pc) ;
	return OK ;
}

//链表多项式减法
Status Poly_sub(polynomail pa , polynomail pb , polynomail &pc){
	polynomail la = pa -> next ;
	polynomail lb = pb -> next ;
	polynomail lc ;
	pc = pa ;
	lc = pa ;
	while(la && lb){
		if(la -> data.expn < lb -> data.expn){
			lc -> next = la ;
			lc = la ;
			la = la -> next ;
		}
		else if(la -> data.expn < lb -> data.expn){
			lb -> data.coef = - lb -> data.coef ;
			lc -> next = lb ;
			lc = lb ;
			lb = lb -> next ;
		}
		else if (la -> data.expn == lb -> data.expn){
			la -> data.coef = la -> data.coef - lb -> data.coef ;
			lc -> next = la ;
			lc = la ;
			la = la -> next ;
			lb = lb -> next ;
		}
	}
	if(la) lc -> next = la ;
	else{
		polynomail lb1 = lb ;
		while(lb){
			lb -> data.coef = -lb -> data.coef ;
			lb = lb -> next ;
		}
		lc -> next = lb1 ; 
	}
	free(pb) ;
	print_p(pc) ;
	DestroyPolynomial(pc) ;
	return OK ;
}

//链表多项式乘法
Status Poly_multiply(polynomail pa , polynomail pb){
	polynomail pc ;
	CreatPolynomial(pc) ;
	polynomail p1 = pa ;
	polynomail p2 = pb ;
	term tmp ;
	int expn ;
	double coef ;
	while(p1 -> next){
		while(p2 -> next){
			expn = p1 -> next -> data.expn + p2 -> next -> data.expn ;
			coef = p1 -> next -> data.coef * p2 -> next -> data.coef ;
			tmp.expn = expn ;
			tmp.coef = coef ;
			Poly_Insert_tail(pc , tmp) ;
			p2 = p2 -> next ;
		}
		p1 = p1 -> next ;
		p2 = pb ;
	} //第一步完成所有插入，但此时同次数的尚未合并
	polynomail p3 = pc ;
	polynomail p4 ;
	while(p3 -> next){
		p4 = p3 -> next ;
		while(p4 -> next){
			if(p4 -> next -> data.expn == p3 -> next -> data.expn){
				p3 -> next -> data.coef = p3 -> next -> data.coef + p4 -> next -> data.coef ;
				polynomail t = p4 -> next ;
				p4 -> next = p4 -> next -> next ;
				free(t) ;
			}
			p4 = p4 -> next ;
		}
		p3 = p3 -> next ;
	}
	free(pa) ;
	free(pb) ;
	print_p(pc) ;
	DestroyPolynomial(pc) ;
	return OK ;
}

//链表多项式求导
Status Poly_derivation(polynomail &P , int n){
	polynomail p = P ;
	double coef ;
	int expn ;
	while(p -> next) p = p -> next ; //先将p移动到最后一项
	if(p -> data.expn < n){
		cout << "所求导数为0" << endl ;
		return OK ;
	}
	p = P ;
	while(p -> next){
		expn = p -> next -> data.expn ;
		coef = p -> next -> data.coef ;
		for(int i = 0 ; i < n ; i++){
			coef *= expn ;
			expn-- ;
		}
		p -> next -> data.expn = p -> next -> data.expn - n ;
		p -> next -> data.coef = coef ;
		p = p -> next ;
	}
	print_p(P) ;
	return OK ;
}

//初始化一个顺序表
Status InitList_Sq(Sqlist &L){
  L.elem = (ElemType *)malloc (LIST_INIT_SIZE * sizeof(ElemType)) ;
  if( !L.elem ) exit(Overflow) ;
  L.length = 0 ;
  L.listsize = LIST_INIT_SIZE ;
  return OK ;
}

//初始化一个顺序表
Status InitList_P(Sq_p &L){
  L.elem = (term *)malloc (LIST_INIT_SIZE * sizeof(term)) ;
  if( !L.elem ) exit(Overflow) ;
  L.length = 0 ;
  L.listsize = LIST_INIT_SIZE ;
  return OK ;
}

//清空一个顺序表
Status ClearList_Sq(Sqlist &L){
    L.length = 0 ;
    L.listsize = LIST_INIT_SIZE ;
    return OK ;
}

//清空一个顺序表
Status ClearList_P(Sq_p &L){
    L.length = 0 ;
    L.listsize = LIST_INIT_SIZE ;
    return OK ;
}

//查看一个线性表的长度 
Status ListLength_Sq(Sqlist l)
{   
	if(!l.elem)
		exit(Overflow) ;
	return l.length ;
}

//查看一个线性表的长度
Status ListLength_P(Sqlist l) 
{   
	if(!l.elem) exit(Overflow) ;
	return l.length ;
}

//插入元素，成功返回1，不成功返回0 
Status ListInsert_Sq(Sqlist &l,int i,int e){
	if(i < 1 || i > l.length + 1) return ERROR ;
	if(l.length >= l.listsize){
		int* newbase = (int*)realloc(l.elem , (l.listsize+LISTINCREMENT)*sizeof(ElemType));  //realloc申请更大的空间，同时释放原空间 
		if(!newbase) exit(Overflow) ;
		l.elem = newbase ;
		l.listsize = l.listsize + LISTINCREMENT ;
	}
	if(!l.length){
		l.elem[0] = e ;
		l.length++ ;
	//	Show(l);
		return OK ;
	} //如果顺序表中还没有元素，则直接给0赋值
	int* q = &(l.elem[i - 1]) ;
	for(int* p = &(l.elem[l.length - 1]) ; p >= q ; --p){
        *(p + 1) = *p ;
    }
	*q = e ;
	++l.length ;
	return OK ;
}

//插入多项式元素 
Status ListInsert_P(Sq_p &l , int i , term e){
	if(i < 1 || i > l.length + 1) return ERROR ;
	if(l.length >= l.listsize){
		term* newbase = (term*)realloc(l.elem , (l.listsize+LISTINCREMENT) * sizeof(term));  //realloc申请更大的空间，同时释放原空间 
		if(!newbase) exit(Overflow) ;
		l.elem = newbase ;
		l.listsize = l.listsize + LISTINCREMENT ;
	}
	if(!l.length){
		l.elem[0].coef = e.coef ;
		l.elem[0].expn = e.expn ;
		l.length++ ;
		return OK ;
	}
	term* q = &(l.elem[i - 1]) ;
	for(term* p = &(l.elem[l.length - 1]) ; p >= q ; --p){
	*(p + 1) = *p ;
	}
	*q = e ;
	++l.length ;
	return OK ;
}

//在顺序线性表L中刷除第i个元素，并用e返回其值
Status ListDelete_Sq ( Sqlist &l, int i, ElemType &e){
	if (i < 1 || i>l.length ) return ERROR ;
	ElemType *p = &l.elem[i - 1] ;
	e = *p ;//取第i个元素的值
	ElemType *q = l.elem + l.length - 1 ; //表尾
	for(++p ; p <= q ; ++p){
		*(p - 1) = *p ;
	}
	--l.length ;
	return OK ;
}

//删除多项式元素
Status ListDelete_P(Sq_p &l , int i , term e){
	if(i < 1 || i > l.length) return ERROR ;
	term* p = &(l.elem[i - 1]) ;
	e = *p ;
	term* q = l.elem + l.length - 1;
	for(++p ; p <= q ; ++p) *(p - 1) = *p ;
	--l.length ;
	return OK ;
}

//线性表向量的加法运算
Sqlist add(Sqlist l1 , Sqlist l2){
	Sqlist add_Sqlist ;
	InitList_Sq(add_Sqlist) ;
	for(int i = 0 ; i < l1.length ; i++){
		ListInsert_Sq(add_Sqlist , i + 1 , l1.elem[i] + l2.elem[i]);
	}
	return add_Sqlist ;
}

//线性表向量的减法运算
Sqlist sub(Sqlist l1 , Sqlist l2){
	Sqlist sub_Sqlist ;
	InitList_Sq(sub_Sqlist) ;
	for(int i = 0 ; i < l1.length ; i++){
		ListInsert_Sq(sub_Sqlist , i + 1 , l1.elem[i] - l2.elem[i]);
	}
	return sub_Sqlist ;
}

//线性表向量的乘法运算
Sqlist multiply(Sqlist l1, Sqlist l2){
	Sqlist mul_Sqlist ;
	InitList_Sq(mul_Sqlist) ;
	for(int i = 0 ; i < l1.length ; i++){
		ListInsert_Sq(mul_Sqlist , i + 1 , l1.elem[i] * l2.elem[i]);
	}
	return mul_Sqlist ;
}

//向量的夹角余弦值
double cos_value(Sqlist l1 , Sqlist l2){
	double x = 0 ;
	double y = 0 ;
	double z = 0 ;
	int length = l1.length ; 
	for(int i = 0 ; i <= length ; i++){
		x += l1.elem[i] * l2.elem[i] ;
		y += l1.elem[i] * l1.elem[i] ;
		z += l2.elem[i] * l2.elem[i] ;
	}
	return (x / (sqrt(y) * sqrt(z))) ;
}

//展示多项式
void Show(Sqlist l){
	cout << "(" ; 
	for(int i = 0 ; i < l.length ; i++){
		if(i < l.length - 1)
			cout << l.elem[i] << "," ;
		else
			cout << l.elem[i] << ")" << endl ;
	}
}

//按照次数，排序一元多项式顺序表
Status Sort_p(Sq_p &l){
	int j , k ;
	for(int i = 0 ; i < l.length ; i++){
		k = i ;
		for(int j = i + 1 ; j < l.length ; j++){
			if(l.elem[k].expn > l.elem[j].expn) k = j ;
		}
		if (k != i){
			term tmp ;
			tmp = l.elem[i] ; 
			l.elem[i] = l.elem[k] ;
			l.elem[k] = tmp ;
		}
	}
	return OK ;
}

//渲染多项式并输出
void show_p(Sq_p l){
	for(int i = 0 ; i < l.length ; i++){
		if(i < l.length - 1){
			if(l.elem[i].expn){
				printf("%.2fx^%d + " , l.elem[i].coef, l.elem[i].expn) ;
			}
			else printf("%.2f + " , l.elem[i].coef) ;
		}
		else{
			if(l.elem[i].expn){
				printf("%.2fx^%d" , l.elem[i].coef, l.elem[i].expn) ;
			}
			else printf("%.2f" , l.elem[i].coef) ;
		}
	}
}

//多项式加法
Status add_p(Sq_p l1 , Sq_p l2){
	Sq_p ans_p ;
	InitList_P(ans_p) ;
	double coef = 0 ;
	int i = 0 ; //l1中正在进行运算的元素
	int j = 0 ; //l2中正在进行运算的元素
	int k = 1 ; //指示结果的多项式中元素进行的个数
	while(i < l1.length && j < l2.length){
		if(l1.elem[i].expn < l2.elem[j].expn){
			ListInsert_P(ans_p , k++ , l1.elem[i]) ;
			i++ ;
		}
		else if(l1.elem[i].expn > l2.elem[j].expn){
			ListInsert_P(ans_p , k++ , l2.elem[j]) ;
			j++ ;
		}
		else {
			term tmp ;
			tmp.coef = l1.elem[i].coef + l2.elem[j].coef ;
			tmp.expn = l1.elem[i].expn ;
			if(tmp.coef) ListInsert_P(ans_p , k++ , tmp) ;
			i++ ;
			j++ ;
		}
	}
	if(i >= l1.length){
		for(j = j ; j < l2.length ; j++) ListInsert_P(ans_p , k++ , l2.elem[j]) ;
	}
	else {
		for(i = i ; i < l1.length ; i++) ListDelete_P(ans_p , k++ , l1.elem[i]) ;
	}
	show_p(ans_p) ;
	return OK ;
}

//多项式减法
Status sub_p(Sq_p l1 , Sq_p l2){
	Sq_p ans_p ;
	InitList_P(ans_p) ;
	double coef = 0 ;
	int i = 0 ; //l1中正在进行运算的元素
	int j = 0 ; //l2中正在进行运算的元素
	int k = 1 ; //指示结果的多项式中元素进行的个数
	while(i < l1.length && j < l2.length){
		if(l1.elem[i].expn < l2.elem[j].expn){
			ListInsert_P(ans_p , k++ , l1.elem[i]) ;
			i++ ;
		}
		else if(l1.elem[i].expn > l2.elem[j].expn){
			l2.elem[j].coef = - l2.elem[j].coef ;
			ListInsert_P(ans_p , k++ , l2.elem[j]) ;
			j++ ;
		}
		else {
			term tmp ;
			tmp.coef = l1.elem[i].coef - l2.elem[j].coef ;
			tmp.expn = l1.elem[i].expn ;
			if(tmp.coef) ListInsert_P(ans_p , k++ , tmp) ;
			i++ ;
			j++ ;
		}
	}
	if(i >= l1.length){
		l2.elem[j].coef = - l2.elem[j].coef ;
		for(j = j ; j < l2.length ; j++) ListInsert_P(ans_p , k++ , l2.elem[j]) ;
	}
	else {
		for(i = i ; i < l1.length ; i++) ListDelete_P(ans_p , k++ , l1.elem[i]) ;
	}
	show_p(ans_p) ;
	return OK ;
}

//查找函数：用于返回已经存入指数的位置，否则返回空置的第一个位置。
int Search_function(term* a , int expn){
	int j = 0 ;
	for(int i = 0 ; i < 100 ; i++){
		if(a[i].expn == expn) return i ;
		else if(a[i].expn == 0 && a[i].coef == 0){
			j = i ;
			break ;
		}
	}
	return j ;
}

//多项式乘法
Status multiply_p(Sq_p l1 , Sq_p l2){
	term tmp_p_array[100] ; //开一个临时的存放每一项的数组，最大范围与预定义的相同。
	Sq_p ans_p ;
	int expn ;
	double coef ;
	InitList_P(ans_p) ;
	for(int i = 0 ; i < 100 ; i++){
		tmp_p_array[i].coef = 0 ;
		tmp_p_array[i].expn = 0 ;
	} //完成临时项数组的初始化
	for(int i = 0 ; i < l1.length ; i++){
		for(int j = 0 ; j < l2.length ; j++){
			expn = l1.elem[i].expn * l2.elem[j].expn ;
			coef = l1.elem[i].coef * l2.elem[j].coef ;
			int t = Search_function(tmp_p_array , expn) ;
			tmp_p_array[t].expn = expn ;
			tmp_p_array[t].coef += coef ;
		}
	}
	int k = 1 ;
	for(int i = 0 ; i < 100 ; i++){
		if(tmp_p_array[i].coef){
			ListInsert_P(ans_p , k++ , tmp_p_array[i]) ;
		}
	}
	Sort_p(ans_p) ; 
	show_p(ans_p) ;
	return OK ; 
}

//多项式n次求导
Status polyder_p(Sq_p l , int n){
	term tmp_p_array[100] ;
	Sq_p ans_p ;
	int expn = 0 ;
	double coef = 0 ;
	InitList_P(ans_p) ;
	for(int i = 0 ; i < 100 ; i++){
		tmp_p_array[i].coef = 0 ;
		tmp_p_array[i].expn = 0 ;
	}
	if(n > l.elem[l.length - 1].expn){
		cout << "该多项式的" << n << "次导数为0。" << endl ;
		return OK ; 
	}
	for(int i = 0 ; i < l.length ; i++){
		if(l.elem[i].expn >= n){
			expn = l.elem[i].expn ;
			coef = l.elem[i].coef ;
			for(int j = 0 ; j < n ; j++){
				coef *= expn ;
				expn-- ;
			}
			int t = Search_function(tmp_p_array , expn) ;
			tmp_p_array[t].coef = coef ;
			tmp_p_array[t].expn = expn ;
		}
	}
	for(int i = 0 ; ; i++){
		if(tmp_p_array[i].coef == 0) break ;
		ListInsert_P(ans_p , i + 1 , tmp_p_array[i]) ;
	}
	show_p(ans_p) ;
	return OK ;
}

//栈OPTR
typedef struct{
	char* base ;
	char* top ;
	int stacksize ;
}SqStack1 ;

//栈OPND
typedef struct{
	double* base ;
	double* top ;
	int stacksize ; 
}SqStack2 ;

//初始化栈1
void InitStack1(SqStack1 &s){
	s.base = (char*)malloc(STACK_INIT_SIZE * sizeof(char)) ;
	if(!s.base) exit(Overflow) ;
	s.top = s.base ;
	s.stacksize = STACK_INIT_SIZE ;
	return ;
}

//初始化栈2
void InitStack2(SqStack2 &s){
	s.base = (double*)malloc(STACK_INIT_SIZE * sizeof(double)) ;
	if(!s.base) exit(Overflow) ;
	s.top = s.base ;
	s.stacksize = STACK_INIT_SIZE ;
	return ;
}

//清空栈1
void Clear_Stack1(SqStack1 &S){
	S.top = S.base ;
	free(S.base) ;
	S.stacksize = 0 ;
}

//清空栈2
void Clear_Stack2(SqStack2 &S){
	S.top = S.base ;
	free(S.base) ;
	S.stacksize = 0 ;
}

//获得栈1的栈顶元素
char GetTop1(SqStack1 s){
	if(s.top == s.base){
		cout << "ERROR When get top" << endl ;
		return '0' ;
	}
	char e = *(s.top - 1) ;
	return e ;
}

//获得栈2的栈顶元素
double GetTop2(SqStack2 s){
	if(s.top == s.base){
		printf("ERROR When get top\n") ;
		return 0 ;
	}
	double e = *(s.top - 1) ; //top在栈顶元素的下一个位置上，所以
	return e ;
}

//进栈
void Push1(SqStack1 &s , char e){
	if(s.top - s.base >= s.stacksize){
		s.base = (char*)realloc(s.base , (s.stacksize+STACKINCREMENT) * sizeof(char)) ;
		if(!s.base) exit(Overflow) ;
		s.top = s.base + s.stacksize ;
		s.stacksize += STACKINCREMENT ;
	}
	*s.top++ = e ;
	return ;
}

//进栈
void Push2(SqStack2 &s , double e){
	if(s.top - s.base >= s.stacksize){
	s.base = (double*)realloc(s.base,(s.stacksize+STACKINCREMENT)*sizeof(double)) ;
	if(!s.base) exit(Overflow) ;
	s.top = s.base + s.stacksize ;
	s.stacksize += STACKINCREMENT ;
	}
	*s.top++ = e ;
	return ;
}

//弹出栈顶元素，若栈不空，则删除栈顶的元素，并用e返回元素
void Pop1(SqStack1 &s , char &e){
	if(s.top == s.base){
		cout << "ERROR When pop" << endl ;
		return ;
	}
	e = * --s.top ;
}

//弹出栈顶元素，若栈不空，则删除栈顶的元素，并用e返回元素
void Pop2(SqStack2 &s , double &e){
	if(s.top == s.base){
		cout << "ERROR When pop" << endl ;
		return ;
	}
	e = * --s.top ;
}

//创建一个矩阵
Matrix CreatMatrix(){
	Matrix m ;
	int row , col ;
	cout << "请输入行数和列数：" << endl ;
	cin >> row >> col ;
	double **saveMatrix ;
	saveMatrix=(double **) malloc(row * sizeof(double*)) ;
	for(int i = 0 ; i < row ; i++){
		saveMatrix[i] = (double *) malloc(col * sizeof(double*)) ;
	}
	cout << "请输入矩阵的值：" << endl ;
	for(int i = 0 ; i < row ; i++){
		for(int j = 0 ; j < col ; j++){
			cin >> saveMatrix[i][j] ;
		}
	}
	m.col = col ;
	m.row = row ;
	m.matrix = saveMatrix ;
	return m ;
}

//初始化一个行为row列为col矩阵，其中所有元素为0
Matrix InitMatrix(int row,int col)
{
    Matrix m ;
    double **matrix ;
    matrix = (double**) malloc (row * sizeof(double*)) ;
    for(int i = 0 ; i < row ; i++)
        matrix[i] = (double *)malloc(col * sizeof(double)) ;
    for(int i = 0 ; i < row ; i++)
    {
        for(int j = 0 ; j < col ; j++)
        {
            matrix[i][j] = 0 ;
        }
    }
    m.col = col ;
    m.row = row ;
    m.matrix = matrix ;
    return m ;
}

//销毁矩阵
void DestroyMatrix(Matrix &m){
	int n = m.row * m.col ;
	for(int i = 0 ; i < n ; i++){
		free(m.matrix[i]) ;
	}
}

//矩阵加法
Matrix add(Matrix m1, Matrix m2)
{
	Matrix ans ;
    for(int i = 0 ; i < m1.row ; i++)
    {
        for(int j = 0 ; j < m1.col ; j++)
        {
            ans.matrix[i][j] = m1.matrix[i][j] + m2.matrix[i][j] ;
        }
    }
    return ans ;
}

//矩阵减法
Matrix sub(Matrix m1, Matrix m2)
{
	Matrix ans ;
    for(int i = 0 ; i < m1.row ; i++)
    {
        for(int j = 0 ; j < m1.col ; j++)
        {
            m1.matrix[i][j] = m1.matrix[i][j] - m2.matrix[i][j] ;
        }
    }
    return ans ;
}

//计算行与列相乘之和
double calrowcolMul(Matrix m1 , Matrix m2 , int row , int col){
	double result = 0 ;
	int same = m1.col ;
	for(int j = 0 ; j < same ; j++){
		result += (m1.matrix[row][j] * m2.matrix[j][col]) ;
	}
	return result ;
}

//矩阵乘法
Matrix mul(Matrix m1 , Matrix m2){
	Matrix result = InitMatrix(m1.row , m2.col) ;
	for(int i = 0 ; i < m1.row ; i++){
		for(int j = 0 ; j < m2.col ; j++){
			result.matrix[i][j] = calrowcolMul(m1 , m2 , i , j) ;
		}
	}
	return result ;
}

//矩阵的数乘
Matrix numMul(Matrix m , double num)
{
	Matrix result = InitMatrix(m.row , m.col) ;
    cout << "数值:" << num << endl ;
    for(int i = 0 ; i < m.row; i++)
    {
        for(int j = 0 ; j < m.col ; j++)
        {
            result.matrix[i][j] = m.matrix[i][j] * num ;
        }
    }
    return result ;
}

//高斯列主元方法求行列式
double Det(Matrix m){
	Matrix A = m ;
	int n = m.row ;
	Matrix tmp = m ; //保存原数组，结束后要复原用
	if(m.row != m.col){
		cout << "非合法矩阵，无法求行列式" << endl ;
		return 0 ;
	}
	double det = 1 ;
	double t ;
	for(int k = 0 ; k < n - 1 ; k++){
		double max = m.matrix[k][k] ;
		int Ik = k ;
		for(int j = k + 1 ; j < n ; j++){
			if(max <= m.matrix[j][k]){
				max = m.matrix[j][k] ;
				Ik = j ;
			}
		}
		if(max == 0){
			return 0 ;
		}
		if(Ik != k){
			for(int j = k ; j < n ; j++){
				t = m.matrix[Ik][j] ;
				m.matrix[Ik][j] = m.matrix[k][j] ;
				m.matrix[k][j] = t ; 
			}
			det *= -1 ;
		}
		for(int i = k + 1 ; i < n ; i++){
			t = m.matrix[i][k] / m.matrix[k][k] ;
			m.matrix[i][k] = t ;
			for(int j = k + 1 ; j < n ; j++){
				m.matrix[i][j] = m.matrix[i][j] - t * m.matrix[k][j] ;
			}
		}
		det *= m.matrix[k][k] ;
	}
	if(m.matrix[n - 1][n - 1] == 0){
		cout << "该矩阵的行列式为:0" << endl ; 
		return 0 ;
	}
	else{
		cout << "该矩阵的行列式为:" << det * m.matrix[n - 1][n - 1] << endl ;
		double ans = det * m.matrix[n - 1][n - 1] ;
		m = tmp ;
		return ans ;
	}
}

//判断是否为奇异矩阵
int NonSingularMatrix(Matrix m){
	if(Det(m) != 0) return 1 ;
	return 0 ;
}

//符号函数
int sgn(double x){
	if(x > 0) return 1 ;
	else if(x == 0) return 0 ;
	else return -1 ;
}

//判断数组是否全部0
int IsZero(double a[] , int j){
	for(int i = 0 ; i < j ; i++){
		if(a[i] != 0) return 0 ;
	}
	return 1 ;
}

//矩阵输出
void printMatrix(Matrix m)
{
    for(int i = 0 ; i < m.row ; i++)
    {
        for(int j = 0 ; j < m.col ; j++)
        {
            cout << m.matrix[i][j] << "  ";
        }
        cout << endl ;
    }
}

//将原矩阵化为Hessenberg矩阵
Matrix Hessenberg(Matrix m){
	int n = m.row ;
	double T[n][n] ;
	double B[n][n] ;
	double C[n][n] ;
	double R[n - 1][n - 1] ;
	double I[n - 1][n - 1] ;
	double c[n] ;
	double v[n] ;
	double u[n] ;
	double t , w , s ;
	int i , j , k , l ;
	Matrix tmp = m ;
	for(k = 0 ; k < n - 2 ; k++){
		for(i = 0 ; i < n - k - 1 ; i++){
			for(j = 0 ; j < n - k - 1 ; j++){
				if(i == j) I[i][j] = 1 ;
				else I[i][j] = 0 ; //定义单位矩阵I
			}
		}
		double max = fabs(m.matrix[k + 1][k]) ;
		for(i = 0 ; i < n - k - 1 ; i++){
			if(max < fabs(m.matrix[i + k + 1][k])) max = fabs(m.matrix[i + k + 1][k]) ;
		//求最大值
		}
		for(i = 0 ; i < n - k - 1 ; i++){
			c[i] = m.matrix[i + k + 1][k] / max ; 
		//标准化数组
		}

		if(IsZero(c , n - k - 1)) continue ;
		//若数组为0，则这一步不需要约化

		for(i = 0 , t = 0.0 ; i < n - k - 1 ; i++){
			t += c[i] * c[i] ;
		}

		v[k] = sgn(m.matrix[k+1][k]) * sqrt(t) ;
		u[0] = c[0] + v[k] ;

		for(j = 1 ; j < n - k - 1 ; j++) u[j] = c[j] ;
		w = v[k] * (c[0] + v[k]) ;

		for(i = 0 ; i < n - k - 1 ; i++){
			for(j = 0 ; j < n - k - 1 ; j++) R[i][j] = I[i][j] - u[i] * u[j] / w ;
		}

		for(i = 0 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				if(i == j) T[i][j] = 1 ;
				else T[i][j] = 0 ;
			}
		}

		for(i = 0 ; i < n - k - 1 ; i++){
			for(j = 0 ; j < n - k - 1 ; j++){
				T[i + k + 1][j + k + 1] = R[i][j] ;
			}
		}

		for(i = 0 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(l = 0 , s = 0.0 ; l < n ; l++){
					s += T[i][l] * m.matrix[l][j] ;
				}
			B[i][j] = s ;
			}
		}
		for(i = 0 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				for(l = 0 , s = 0.0 ; l < n ; l++){
					s += B[i][l] * T[l][j] ;
				}
			C[i][j] = s ;
			}
		}
		for(i = 0 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				m.matrix[i][j] = C[i][j] ;
			}
		}
	}
	cout << "原矩阵化为上Hessenberg矩阵为：" << endl ;
	printMatrix(m) ;
	return tmp ;
}

//判断是否为上Hessenberg矩阵
int IsHessenberg(Matrix E){
	int n = E.row ;
	for(int i = 2 ; i < n ; i++){
		for(int j = 0 ; j + 1 < i ; j++){
			if(E.matrix[i][j] != 0) return 0 ;
		}
	}
	return 1 ;
}

//QR算法求特征值
void QRAlgorithm(Matrix m){
	int count = 1 ;
	int n = m.row ;
	int i , j , k , l ;
	double p[n][n] ;
	double Q[n][n] ;
	double R[n][n] ;
	double F[n][n] ;
	double V[n][n] ;
	double c , s , t , y , max ;
	for(int i = 0 ; i < n ; i++){
		for(int j = 0 ; j < n ; j++){
			if(i != j) Q[i][j] = 0 ;
			else Q[i][j] = 1 ;
		}
	}//Q为单位矩阵

	for(l = 0 ; l < n - 1 ; l++){
		for(i = 0 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				if(i != j) p[i][j] = 0 ;
				else p[i][j] = 1 ;
			}
		}
		c = m.matrix[l][l] / sqrt(m.matrix[l][l] * m.matrix[l][l] + m.matrix[l + 1][l] * m.matrix[l + 1][l]) ;
		s = m.matrix[l + 1][l] / sqrt(m.matrix[l][l] * m.matrix[l][l] + m.matrix[l + 1][l] * m.matrix[l + 1][l]) ;
		p[l][l] = c ;
		p[l][l + 1] = s ;
		p[l + 1][l] = -s ;
		p[l + 1][l + 1] = c ;
		for(i = 0 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				t = 0 ;
				y = 0 ;
				for(k = 0 ; k < n ; k++){
					t += p[i][k] * m.matrix[k][j] ;
					y += p[i][k] * Q[k][j] ;
				}
				R[i][j] = t ;
				F[i][j] = y ;
			}
		}
		for(i = 0 ; i < n ; i++){
			for(j = 0 ; j < n ; j++){
				m.matrix[i][j] = R[i][j] ;
				Q[i][j] = F[i][j] ;
			}
		}
	}
	for(i = 0 ; i < n ; i++){
		for(j = 0 ; j < n ; j++) V[i][j] = Q[j][i] ;
	}
	for(i = 0 ; i < n ; i++){
		for(j = 0 ; j < n ; j++) Q[i][j] = V[i][j] ;
	}
	for(i = 0 ; i < n ; i++){
		for(j = 0 ; j < n ; j++){
			t = 0 ;
			for(k = 0 ; k < n ; k++) t += R[i][k] * Q[k][j] ;
			m.matrix[i][j] = t ;
		}
	}
	count ++ ;
	max = fabs(m.matrix[1][0]) ;
	for(i = 1 ; i < n ; i++){
		for(j = 0 ; j < i ; j++){
			if(fabs(m.matrix[i][j]) > max) max = fabs(m.matrix[i][j]) ;
		}
	}
	if(max < eps){
		cout << "在1e-5精度下，原矩阵的全部特征值为：" << endl ;
		for(i = 0 ; i < n ; i++){
			if(i % 3 == 0) cout << '\n' ;
			cout << "a" << i + 1 << '=' << m.matrix[i][i] << '\t' ;
		}
		cout << '\n' ;
		return ;
	}
	QRAlgorithm(m) ;
}

//寻找特征值
void SeekEigenvalue(Matrix m){
	int n = m.row ;
	Matrix Z = InitMatrix(n , n) ;
	for(int i = 0 ; i < n ; i++){
		for(int j = 0 ; j < n ; j++){
			Z.matrix[i][j] = m.matrix[i][j] ;
		}
	}
	if(NonSingularMatrix(Z) == 0){
	    cout << "该矩阵不是非奇异矩阵，不符合分解条件" << endl ;
		return ;
	}
	else cout << "该矩阵时非奇异矩阵，可求取特征值" << endl ;
	if(IsHessenberg(m) == 0){
		Hessenberg(m) ;
	}
	cout << "QR分解求取特征值：" << endl ;
	QRAlgorithm(m) ;
}

//判断输入的字符是不是运算符,是则返回1否则返回0
int In(char e){
	if(e == '+' || e =='-' || e == '*' || e == '/' || e == '(' || e == ')' || e == '#') return 1 ;
	else return 0 ; 
}

//判断输入的字符是不是字母是则返回1否则返回0
int Letter(char e){
	if(e >= 'a' && e <= 'z' || (e >= 'A' && e <= 'Z')) return 1 ;
	else return 0 ; 
}

//比较运算符的优先级
char Compare(char a , char b){
	char ans ;
	if(a == '+' || a == '-'){
		if(b == '+' || b == '-' || b == ')' || b == '#') ans = '>' ;
		else if (b == '*' || b == '/' || b == '(') ans = '<' ;
	}
	else if(a == '*' || a == '/'){
		if(b == '+' || b == '-' || b == '*' || b == '/' || b == ')' || b == '#') ans = '>' ;
		else if(b == '(') ans = '<' ;
	}
	else if(a == '('){
		if(b == '+' || b == '-' || b == '*' || b == '/' || b == '(') ans = '<' ;
		else if(b == ')') ans = '=' ;
	}
	else if(a == ')'){
		if(b == '+' || b == '-' || b == '*' || b == '/' || b == ')' || b =='#') ans = '>' ;
	}
	else if(a == '#'){
		if(b == '+' || b == '-' || b == '*' || b == '/' || b =='(') ans = '<' ;
		else if(b == '#') ans = '=' ;
	}
	return ans ;
}

//模拟计算过程
double calculate(double a , char op , double b){
	double ans ;
	if(op == '+') ans = a + b ;
	else if(op == '-') ans = a - b ;
	else if(op == '*') ans = a * b ;
	else if(op == '/') ans = a / b ;
	return ans ;
}

//字符串转double
double str2double(string str){
	bool demical = false ;
	double count = 0 ;
	double d = 0 ;
	double e = 0 ;
	if(str[0] == '-'){
		for(int index = 1 ; index < str.length() ; index++){
			int k = index ;
			for(int j = k ; str[j] != '.' && j < str.length() ; j++){
				count++ ;
			}
			for(;count > 0 ; count--){
				e += (str[index] - '0') * pow(10 , count - 1) ;
				index++ ; 
			}
			count = 0 ;
			if(str[index] == '.'){
				demical = true ;
			}
			if(demical){
				for(int j = index + 1 ; j < str.length() ; j++){
					count += 1 ;
					e += double(str[j] - '0') / pow(10,count) ;
					k = j ;
				}
				break ;
			}
		}
		e = -e ;
	}
	else{
		for(int index = 0 ; index < str.length() ; index++){
			int k = index ;
			for(int j = k ; str[j] != '.' && j < str.length() ; j++){
				count++ ;
			}
			for(;count > 0 ; count--){
				e += (str[index] - '0') * pow(10 , count - 1) ;
				index++ ; 
			}
			count = 0 ;
			if(str[index] == '.'){
				demical = true ;
			}
			if(demical){
				for(int j = index + 1 ; j < str.length() ; j++){
					count += 1 ;
					e += double(str[j] - '0') / pow(10,count) ;
					k = j ;
				}
				break ;
			}
		}
		e = e ;
	}
	return e ;
}

//算术表达式求值的算符优先算法
double EvaluateExpression(string str){
	SqStack1 OPTR ; //存放操作符
	SqStack2 OPND ; //存放运算数
    SString tmp[20] ;
	char x , op ;
	double a = 0 ;
	double b = 0 ;
	InitStack1(OPTR) ;
	InitStack2(OPND) ;
	Push1(OPTR , '#') ;
	bool decimal = false ;
	int i = 0 ; //表达式遍历下标
    int j = 0 ; //这是存放多变量字符串的tmp数组的遍历下标
    int flag = 0 ;
	int flag2 = 0 ;
	int length = str.length() ;
	str += '#' ;
	bool demical = false ;
	double count = 0 ;
	double d = 0 ;
	while(str[i] != '#' || GetTop1(OPTR)!= '#'){
		if(str[i] == '#' && GetTop1(OPTR) == '#') break ;
		if(!In(str[i]) && !Letter(str[i])){
			int k = i ;
			flag = 1 ; //flag标志着已经遇到了新的数字，但由于不知道是多少位，从k = i位开始直到不再是数字停下。
			for(int j = k ; str[j] != '.' && !In(str[j]) && !Letter(str[j]) && str[i] != '#' ; j++){
				count ++ ;
			}
			for( ; count > 0 ; count--){
				d += (str[i] - '0') * pow(10 , count - 1) ;
				i++ ;
			}
			count = 0 ;
			if(str[i] == '.'){
				demical = true ;
			}
			if(demical){
				for(int j = i + 1 ; !In(str[j]) && !Letter(str[j]) ; j++){
					count += 1 ;
					d += double(str[j] - '0') / pow(10,count) ;
					k = j ;
				}
				count = 0 ;
				if (k != str.length() - 1) i = k + 1 ;
				else i = k ;
			}
			if(flag == 1){
				Push2(OPND , d) ;
				flag = 0 ; //置为0
			}
			d = 0 ;
			demical = false ;
		}
		else if(Letter(str[i])){
            int flag1 = -1 ;
            string tmp_var ;
            SString T ;
			while(!In(str[i])){
                tmp_var += str[i] ;
                i++ ;
            }
            T.ch = tmp_var ;
            for(int i = 0 ; i <= j ; i++){
                if(T.ch == tmp[i].ch){
                    flag1 = 1 ;
                    Push2(OPND , tmp[i].var) ;
                    break ;
                }
            }
            if(flag1 != 1){
			    cout << "请输入变量的值：" << endl ;
			    double var = 0 ;
			    cin >> var ;
                T.var = var ;
                tmp[j] = T ;
                j++ ;
                Push2(OPND , T.var) ;
            }
		}
		else if (In(str[i])){
			switch(Compare(GetTop1(OPTR),str[i])){
				case '<' :
					Push1(OPTR , str[i]) ;
					if(i < str.length() - 1) i++ ;
					break ;
				case '>' :
					Pop1(OPTR , op) ;
					Pop2(OPND , b) ;
					Pop2(OPND , a) ; 
					Push2(OPND , calculate(a , op , b)) ;
					break ;
				case '=' :
					Pop1(OPTR , x) ;
					if(i < str.length() - 1) i++ ;
					break ;
			}
		}
	}
	double ans = GetTop2(OPND) ;
	Clear_Stack1(OPTR) ;
	Clear_Stack2(OPND) ;
	return ans ;
}

//算术表达式求值的算符优先算法
double EvaluateExpression2(string str ,string str1){
	SqStack1 OPTR ; //存放操作符
	SqStack2 OPND ; //存放运算数
    SString tmp[20] ;
	char x , op ;
	double a = 0 ;
	double b = 0 ;
	InitStack1(OPTR) ;
	InitStack2(OPND) ;
	Push1(OPTR , '#') ;
	bool decimal = false ;
	int i = 0 ; //表达式遍历下标
    int j = 0 ; //这是存放多变量字符串的tmp数组的遍历下标
    int flag = 0 ;
	bool demical = false ;
	double count = 0 ;
	double d = 0 ;
	double e = 0 ;
	int length = str.length() ;
	str += '#' ;
	e = str2double(str1) ;
	while(str[i] != '#' || GetTop1(OPTR)!= '#'){
		if(str[i] == '#' && GetTop1(OPTR) == '#') break ;
		if(!In(str[i]) && !Letter(str[i])){
			int k = i ;
			flag = 1 ; //flag标志着已经遇到了新的数字，但由于不知道是多少位，从k = i位开始直到不再是数字停下。
			for(int j = k ; str[j] != '.' && !In(str[j]) && !Letter(str[j]) && str[i] != '#' ; j++){
				count++ ;
			}
			for( ; count > 0 ; count--){
				d += (str[i] - '0') * pow(10 , count - 1) ;
				i++ ;
			}
			count = 0 ;
			if(str[i] == '.'){
				demical = true ;
			}
			if(demical){
				for(int j = i + 1 ; !In(str[j]) && !Letter(str[j]) ; j++){
					count += 1 ;
					d += double(str[j] - '0') / pow(10,count) ;
					k = j ;
				}
				count = 0 ;
				if (k != str.length() - 1) i = k + 1 ;
				else i = k ;
			}
			if(flag == 1){
				Push2(OPND , d) ;
				flag = 0 ; //置为0
			}
			d = 0 ;
			demical = false ;
		}
		else if(Letter(str[i])){
            int flag1 = -1 ;
            string tmp_var ;
            SString T ;
			while(!In(str[i])){
                tmp_var += str[i] ;
                i++ ;
            }
            T.ch = tmp_var ;
            for(int i = 0 ; i <= j ; i++){
                if(T.ch == tmp[i].ch){
                    flag1 = 1 ;
                    Push2(OPND , tmp[i].var) ;
                    break ;
                }
            }
            if(flag1 != 1){
			    double var = 0 ;
				var = e ;
				e = 0 ;
                T.var = var ;
                tmp[j] = T ;
                j++ ;
                Push2(OPND , T.var) ;
            }
		}
		else if (In(str[i])){
			switch(Compare(GetTop1(OPTR),str[i])){
				case '<' :
					Push1(OPTR , str[i]) ;
					if(i < str.length() - 1) i++ ;
					break ;
				case '>' :
					Pop1(OPTR , op) ;
					Pop2(OPND , b) ;
					Pop2(OPND , a) ; 
					Push2(OPND , calculate(a , op , b)) ;
					break ;
				case '=' :
					Pop1(OPTR , x) ;
					if(i < str.length() - 1) i++ ;
					break ;
			}
		}
	}
	double ans = GetTop2(OPND) ;
	Clear_Stack1(OPTR) ;
	Clear_Stack2(OPND) ;
	return ans ;
}

//编程计算
void ProgrammingEvaluate(){
	int flag ;
	int i = 0 ;
	int j = 0 ;
	string str ;
	string Expression ;
	SString tmp ;
	string Expression_array[20] ;
	Func Func[20] ;
	Expression = '\n' ;
	getline(cin , Expression) ;
	getline(cin , Expression) ;
	while(Expression != "END"){
		int index1 = Expression.find("DEF") ;
		int index2 = Expression.find("RUN") ;
		if(index1 != -1){
			for(j = index1 + 4 ; Expression[j] != '=' ; j++){
				Func[i].ch += Expression[j] ;
			}
			string exp_tmp = Expression.substr(j+1) ;
			for(int k = 0 ; k <= i ; k++){
				int index_sub = exp_tmp.find(Func[k].ch) ; 
				if(index_sub != -1){
					string tmp1 = Func[k].expression ;
					string tmp_before = exp_tmp.substr(0 , index_sub) ;
					string tmp_after = exp_tmp.substr(index_sub + Func[k].ch.length()) ;
					string tmp3 = exp_tmp.substr(0,exp_tmp.find(Func[k].ch)) + '(' + tmp1 + ')' + exp_tmp.substr(index_sub + Func[k].ch.length()) ;
					exp_tmp = tmp3 ;
				}
			}
			if(exp_tmp.find('^') != -1){
				int expm = exp_tmp[exp_tmp.find('^') + 1] - '0' ;
				int index ;
				string exp_tmp0 = '(' + exp_tmp.substr(0 , exp_tmp.find('^')) + ')' ;
				string exp_tmp1 = exp_tmp0 ;
				for(int i = 0 ; i < expm - 1 ; i++){
					exp_tmp1 += '*' + exp_tmp0 ;
				}
				exp_tmp = exp_tmp1 ;
			}
			Func[i].expression = exp_tmp ;
			cout << "你定义了第" << i + 1 << "个多项式：" << Func[i].ch << '=' << Func[i].expression << endl ;
			i++ ;
		}
		if(index2 != -1){
			for(int k = 0 ; k <= i ; k++){
				if(Expression[4] == Func[k].ch[0]){
					string tmp ;
					int tmp0 = 6 ;
					while(Expression[tmp0] != ')'){
						tmp += Expression[tmp0] ;
						tmp0++ ;
					}
					double ans = EvaluateExpression2(Func[k].expression , tmp) ;
					cout << "结果为：" << ans << endl ;
					break ;
 				}
			}
		}
		getline(cin , Expression) ;
	}
}

//编程矩阵计算
void ProgrammingMatrixEvaluate(){
	string Expression ;
	Matrix matrix[20] ;
	int ind[2] = {0};
	Expression = '\n' ;
	int i = 0 ;
	cout << "进入矩阵编程模式:" << endl ;
	getline(cin , Expression) ;
	getline(cin , Expression) ;
	while(Expression != "END"){
		int index1 = Expression.find("DEF") ;
		if(index1 != -1){
			matrix[i] = CreatMatrix() ;
			matrix[i].var_name = Expression.substr(4) ;
			cout << matrix[i].var_name << endl ;
			printMatrix(matrix[i]) ;
			i++ ;
		}
		else if(Expression.find("DET") != -1){
			string tmp = Expression.substr(4) ;
			for(int k = 0 ; k <= i ; k++){
				if(tmp == matrix[k].var_name){
					int n = matrix[k].row ;
					double a[n][n] ;
					for(int i = 0 ; i < n ; i++){
						for(int j = 0 ; j < n ; j++){
							a[i][j] = matrix[k].matrix[i][j] ;
						}
					}
					Matrix tmp1 = matrix[k] ;
					Det(matrix[k]) ;
					for(int i = 0 ; i < n ; i++){
						for(int j = 0 ; j < n ; j++){
							matrix[k].matrix[i][j] = a[i][j] ;
						}
					}
					break ;
				}
		    }
		}
		else if(Expression.find("EIG") != -1){
			string tmp = Expression.substr(4) ;
			for(int k = 0 ; k <= i ; k++){
				if(tmp == matrix[k].var_name){
					int n = matrix[k].row ;
					double a[n][n] ;
					for(int i = 0 ; i < n ; i++){
						for(int j = 0 ; j < n ; j++){
							a[i][j] = matrix[k].matrix[i][j] ;
						}
					}
					SeekEigenvalue(matrix[k]) ;
					for(int i = 0 ; i < n ; i++){
						for(int j = 0 ; j < n ; j++){
							matrix[k].matrix[i][j] = a[i][j] ;
						}
					}
					break ;
				}
		    }
		}
		else if(Expression.find("print") != -1){
			string tmp = Expression.substr(6) ;
			for(int k = 0 ; k <= i ; k++){
				if(tmp == matrix[k].var_name){
					printMatrix(matrix[k]) ;
					break ;
				}
		    }
		}
		else if(Expression.find('+') != -1){
			string tmp = Expression.substr(0 , Expression.find('+')) ;
			string tmp2 = Expression.substr(Expression.find('+') + 1) ;
			for(int k = 0 ; k <= i ; k++){
				if(tmp == matrix[k].var_name){
					ind[0] = k ;
				}
				if(tmp2 == matrix[k].var_name){
					ind[1] = k ;
				}
			}
			printMatrix(add(matrix[ind[0]] , matrix[ind[1]])) ;
		}
		else if(Expression.find('-') != -1){
			string tmp = Expression.substr(0 , Expression.find('-')) ;
			string tmp2 = Expression.substr(Expression.find('-') + 1) ;
			for(int k = 0 ; k <= i ; k++){
				if(tmp == matrix[k].var_name){
					ind[0] = k ;
				}
				if(tmp2 == matrix[k].var_name){
					ind[1] = k ;
				}
			}
			printMatrix(sub(matrix[ind[0]] , matrix[ind[1]])) ;
		}
		else if(Expression.find('*') != -1){
			string tmp = Expression.substr(0 , Expression.find('*')) ;
			string tmp2 = Expression.substr(Expression.find('*') + 1) ;
			if(tmp[0] >= '0' && tmp[0] <= '9'){
				double dbl = str2double(tmp) ;
				for(int k = 0 ; k <= i ; k++){
					if(tmp2 == matrix[k].var_name){
						printMatrix(numMul(matrix[k] , dbl)) ;
					}
				}
			}
			else{
				for(int k = 0 ; k <= i ; k++){
					if(tmp == matrix[k].var_name){
						ind[0] = k ;
					}
					if(tmp2 == matrix[k].var_name){
						ind[1] = k ;
					}
				}
				printMatrix(mul(matrix[ind[0]] , matrix[ind[1]])) ;
			}
		}
		getline(cin , Expression) ;
	}
}

//主函数
int main(){
    char mode ;
    string str ;
	int dim = -1 , ans1 = -1 , ans2 = -1 , ans3 = -1 , ans4 = -1 ;
	Sqlist l1 , l2 , l3 ;
	Sq_p l4 , l5 , l6 ;
	polynomail l7 , l8 , l9 ;
	int a[10]={0} ;  //存放向量数据的临时数组 
	term cmp ;  //存放一元多项式数据
	cout << "=========Welcome to my Counter(* ▽* )／========" << endl ;
    cout << "选择计算模式" << endl ;
	cout << "A:表达式计算;B:编程计算;C:编程矩阵运算;" << endl ;
	cout << "D:向量的相关运算;E:用顺序表实现多项式的相关计算;F:用链表实现多项式的相关计算。" << endl ;
    cin >> mode ;
    if(mode == 'A'){
        cout << "请输入表达式：" << endl ;
        cin >> str ;
        double ans = EvaluateExpression(str) ;
        cout << "结果为：" << ans << endl ;
	}	
    if(mode == 'B'){
        cout << "开始进入编程计算" << endl ;
        cout << "输入表达式：" << endl ;
		ProgrammingEvaluate() ;
    }
	if(mode == 'C'){
		ProgrammingMatrixEvaluate() ;
	}
	if(mode == 'D'){
		loop1: cout << "请输入向量的维数：" << endl ;
		cin >> dim ;
		InitList_Sq(l1) ;
		InitList_Sq(l2) ;
		cout << "请输入第一组向量的数据" << endl ;
		for (int i = 0 ; i < dim ; i++){
			cin >> a[i] ;
		}
		for(int i = 1 ; i <= dim ; i++){
			ListInsert_Sq(l1 , i , a[i - 1]) ;
		}
		cout << "你输入的第一组向量为(" ;
		for(int i = 0 ; i < dim ; i++){
			if(i < dim - 1) cout << l1.elem[i] << ',' ;
			else cout << l1.elem[i] << ')' << endl ;
		}
		cout << "请输入第二组向量的数据" << endl ;
		for (int i = 0 ; i < dim ; i++){
			cin >> a[i] ;
		}
		for(int i = 1 ; i <= dim ; i++){
			ListInsert_Sq(l2 , i , a[i - 1]) ;
		}
		cout << "你输入的第二组向量为(" ;
		for(int i = 0 ; i < dim ; i++){
			if(i < dim - 1) cout << l2.elem[i] << ',' ;
			else cout << l2.elem[i] << ')' << endl ;
		}
		cout << "进行加法运算，请输入1，减法运算请输入2，向量的数乘请输入3，计算夹角的余弦值请输入4" << endl ;
		cin >> ans1 ;
		if(ans1 == 1) l3 = add(l1 , l2) ;
		if(ans1 == 2){
			cout << "想让第一个向量为被减数，请输入1，反之请输入0" << endl ;
			cin >> ans2 ;
			if(ans2 == 1) l3=sub(l1 , l2) ;
			else l3 = sub(l2 , l1) ;
		}
		if(ans1 == 3) l3 = multiply(l1 , l2) ;		
		cout << "运算的结果为：" ;
		if(ans1 <= 3) Show(l3);
		else printf("%f\n", cos_value(l1 , l2)) ;
		cout << "现在，所有数据都将被清空。" << endl ;
		ClearList_Sq(l1);
		ClearList_Sq(l2);
		ClearList_Sq(l3);
		ans1 = -1 ; 
		ans2 = -1 ;
		cout << "继续计算向量请输入5,否则输入6" << endl ;
		cin >> ans1 ;
		if(ans1 == 5) goto loop1 ;
		cout << "选择其他模式:" << endl ;
		cout << "E：用顺序表实现多项式的相关计算；F：用链表实现多项式的相关计算；" << endl ;
		cin >> mode ;
	}
	if(mode == 'E'){
		loop2: cout << "请输入第一个一元多项式的项数" << endl ;
		cin >> dim ;
		cout << "请输入第一个一元多项式的系数和次数" << endl ;  //用顺序表存储一元多项式的信息 
		InitList_P(l4) ;
		for(int i = 1;i <= dim ; i++){
			cin >> cmp.coef >> cmp.expn ; //输入一元多项式的系数和次数；
			ListInsert_P(l4 , i , cmp) ;
		}
		Sort_p(l4) ;
		cout << "你输入的多项式为：" ;
		show_p(l4) ;
		cout << endl ;
		cout << "请输入第二个一元多项式的项数" << endl ;
		cin >> dim ;
		cout << "请输入第二个一元多项式的系数和次数" << endl ;  //用顺序表存储一元多项式的信息 
		InitList_P(l5) ;
		for(int i = 1;i <= dim ; i++){
			cin >> cmp.coef >> cmp.expn ; //输入一元多项式的系数和次数；
			ListInsert_P(l5 , i , cmp) ;
		}
		Sort_p(l5) ;
		cout << "你输入的多项式为：" ;
		show_p(l5) ;
		cout << endl ;
		cout << "进行加法运算，请输入1，减法运算请输入2，乘法运算请输入3" << endl ;
		cin >> ans1 ;
		if(ans1 == 1) add_p(l4,l5) ;
		else if(ans1 == 2){
			cout << "想让第一个多项式为被减的多项式，请输入1，反之请输入0" << endl ;
			cin >> ans2 ;
			if(ans2 == 1) sub_p(l4 , l5) ;
			else sub_p(l5 , l4) ;
		}
		else if(ans1 == 3) multiply_p(l4 , l5) ;
		cout << endl ;
		cout << "下面将显示对两个多项式求导的结果" << endl ;
		cout << "请输入第一个多项式的求导次数" << endl ;
		cin >> ans3 ;
		cout << "结果为：" ;
		polyder_p(l4 , ans3) ;
		cout << endl ;
		cout << "请输入第二个多项式的求导次数" << endl ;
		cin >> ans4 ;
		cout << "结果为：" ;
		polyder_p(l5 , ans4) ;
		cout << endl ;
		ans1 = -1 ;
		ans2 = -1 ;
		ans3 = -1 ;
		ans4 = -1 ;
		cout << "继续用顺序表计算多项式请输入5,否则输入6" << endl ;
		cin >> ans1 ;
		if(ans1 == 5) goto loop2 ;
		cout << "选择其他模式:" << endl ;
		cout << "F：用链表实现多项式的相关计算;" << endl ;
		cin >> mode ;
	}
	if(mode == 'F'){
		loop3 : cout << "请输入第一个一元多项式的项数：" << endl ;
		cin >> dim ;
		CreatPolynomial(l7) ;
		cout << "请输入第一个多项式的系数和次数" << endl ;
		for(int i = 0 ; i < dim ; i++){
			cin >> cmp.coef >> cmp.expn ; //输入一元多项式的系数和次数；
			Poly_Insert_tail(l7 , cmp) ;
		}
		sort_poly(l7) ;
		print_p(l7) ;
		cout << "请输入第二个多项式的项数：" << endl ;
		cin >> dim ;
		CreatPolynomial(l8) ;
		cout << "请输入第二个多项式的系数和次数" << endl ;
		for(int i = 0 ; i < dim ; i++){
			cin >> cmp.coef >> cmp.expn ; //输入一元多项式的系数和次数；
			Poly_Insert_tail(l8 , cmp) ;
		}
		sort_poly(l8) ;
		print_p(l8) ;
		CreatPolynomial(l9) ;
		cout << "进行加法运算，请输入1，减法运算请输入2，乘法运算请输入3" << endl ;
		cin >> ans1 ;
		if(ans1 == 1){
			Poly_add(l7 , l8 , l9) ;
		}
		else if(ans1 == 2){
			cout << "想让第一个多项式为被减的多项式，请输入1，反之请输入0" << endl ;
			cin >> ans2 ;
			if(ans2 == 1){
				Poly_sub(l7 , l8 , l9) ;
			}
			else{
				Poly_sub(l8 , l7 , l9) ;
				print_p(l9) ;
			}
		}
		else if(ans1 == 3){
			Poly_multiply(l7 , l8) ;
		}
		DestroyPolynomial(l9) ;
		// cout << "下面将显示对两个多项式求导的结果" << endl ;
		// cout << "请输入第一个多项式的求导次数" << endl ;
		// cin >> ans3 ;
		// Poly_derivation(l7 , ans3) ;
		// cout << endl ;
		// cout << "请输入第二个多项式的求导次数" << endl ;
		// cin >> ans4 ;
		// cout << "结果为：" ;
		// Poly_derivation(l8 , ans4) ;
		// cout << endl ;
		// ans1 = -1 ;
		// ans2 = -1 ;
		// ans3 = -1 ;
		// ans4 = -1 ;
		cout << "继续用链表计算多项式请输入5,否则输入6" << endl ;
		cin >> ans1 ;
		if(ans1 == 5) goto loop3 ;
		cout << "选择其他模式:" << endl ;
		cout << "D：表达式（可含参数）求值" << endl ;
		cin >> mode ;
	}
	return 0 ;
}