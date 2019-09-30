// syntax is:
// list  -> (items)
// items -> item <space> items | item
// item  -> identifier | list
#include <cctype>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>
#include <stack>
#include <memory>

using std::cin;
using std::cout;
using std::endl;
using std::string;
using std::unordered_map;
using std::vector;
using std::stack;
using std::shared_ptr;
using std::make_shared;
using std::weak_ptr;

struct List {
  enum Type {
    E_Define,
    E_Lambda,
    E_Identifier,
    E_BiFunc,
    E_Eq,
    E_Cond,
    E_List,
    E_Number,
    E_Logic,
    E_LambdaArg
  } type;
  string name;
  union {
    bool logic;
    int number;
    int (*func)(int,int);
    int addr;
  } val;
  shared_ptr<List> left, right, identifier_target;
  // use share_ptr will cause circular reference
  weak_ptr<List> parent;
};

struct Token {
  enum State {
    E_Identifier,
    E_Bracket,
    E_Number,
    E_Start,
  } type;
  string name;

  static State move(State &s, char nextchar) {
    switch (s) {
      case E_Identifier:
        break;
      case E_Start:
        if (nextchar == '(') s = E_Bracket;
        else if (nextchar == ')') s = E_Bracket;
        else if (isdigit(nextchar)) s = E_Number;
        else s = E_Identifier;
        break;
      case E_Number:
        if (!isdigit(nextchar)) s = E_Identifier;
        break;
      default:
        s = E_Identifier;
    }
    return s;
  }
};

// Identifier Map
class IdHashMap {
  typedef unordered_map<string, shared_ptr<List> > _Id_Map;
  vector<_Id_Map>  _id;
 public:
  IdHashMap() {
    _id.reserve(64); // function call depth will not exceed 50
    _id.emplace_back();
  }
  void AddTemp() {
    _id.emplace_back();
  }
  void RemoveTemp() {
    _id.pop_back();
  }
  void Put(const string &id, shared_ptr<List> list) {
    _id.back()[id] = list;
  }
  shared_ptr<List> Get(const string &id) {
    for (auto it = _id.rbegin(); it != _id.rend(); ++it) {
      auto res = it->find(id);
      if (res != it->end()) {
        return res->second;
      }
    }
    return nullptr;
  }
};

class Lisp {
  IdHashMap _ids; // identifiers
  shared_ptr<List> _lmd_arg;
  unsigned _pos;
  bool ParseToken(shared_ptr<Token> &token) {
    while (_pos < in.size() && isspace(in[_pos])) {
      ++_pos;
    }
    if (_pos == in.size()) return false;
    token = std::make_shared<Token>();
    token->type = Token::E_Start;
    while (true) {
      token->name.push_back(in[_pos]);
      Token::move(token->type, in[_pos]);
      ++_pos;
      if (token->type == Token::E_Bracket || _pos == in.size() ||
          isspace(in[_pos]) || in[_pos] == ')') {
        return true;
      }
    }
  }
  inline void AddLeft(shared_ptr<List> &cur) {
    cur->left = make_shared<List>();
    cur->left->parent = cur;
    cur = cur->left;
  }
  inline void AddRight(shared_ptr<List> &cur) {
    cur->right = make_shared<List>();
    cur->right->parent = cur;
    cur = cur->right;
  }
  shared_ptr<List> BuildSyntaxTree() {
    _pos = 0;
    vector<shared_ptr<const Token> > token_vec;
    shared_ptr<Token> token;
    while (ParseToken(token)) {
      token_vec.push_back(token);
    }
    // PrintTokens(token_vec);
    stack<shared_ptr<List> > list_stack;
    shared_ptr<List> local_root = make_shared<List>(), cur = local_root;
    for (auto &x: token_vec) {
      cur->name = x->name;
      if (x->type == Token::E_Bracket) {
        if (x->name == "(") {
          cur->name = "list";
          cur->type = List::E_List;
          list_stack.push(cur);
          AddRight(cur);
        } else {
          // right bracket case, pop stack
          if (!cur->parent.expired()) {
            // empty list
            auto p = cur->parent.lock();
            if (p->left == cur) {
              p->left.reset();
            } else {
              p->right.reset();
            }
          }
          cur = list_stack.top();
          list_stack.pop();
          AddLeft(cur);
        }
      } else if (x->type == Token::E_Number) {
        cur->type = List::E_Number;
        cur->val.number = stoi(x->name);
        AddLeft(cur);
      } else if (x->type == Token::E_Identifier) {
        if (x->name == "define") {
          if (!cur->parent.expired()) {
            auto p = cur->parent.lock();
            if (p == list_stack.top()) {
              p->type = List::E_Define;
              p->name = "define";
              continue;
            }
          }
        } else if (x->name == "lambda") {
          if (!cur->parent.expired()) {
            auto p = cur->parent.lock();
            if (p == list_stack.top()) {
              p->type = List::E_Lambda;
              p->name = "lambda";
              continue;
            }
          }
        } else if (x->name == "eq?") {
          if (!cur->parent.expired()) {
            auto p = cur->parent.lock();
            if (p == list_stack.top()) {
              p->type = List::E_Eq;
              p->name = "eq?";
              continue;
            }
          }
        } else if (x->name == "cond") {
          if (!cur->parent.expired()) {
            auto p = cur->parent.lock();
            if (p == list_stack.top()) {
              p->type = List::E_Cond;
              p->name = "cond";
              continue;
            }
          }
        } else {
          cur->type = List::E_Identifier;
        }
        AddLeft(cur);
      }
    }
    local_root->left.reset();
    return local_root;
  }
  void Init() {
    _lmd_arg = make_shared<List>();
    _lmd_arg->type = List::E_LambdaArg;
    auto def = make_shared<List>();
    def->type = List::E_BiFunc;
    def->name = "+";
    def->val.func = [](int x, int y) {return x+y;};
    _ids.Put("+", def);

    def = make_shared<List>();
    def->type = List::E_BiFunc;
    def->name = "-";
    def->val.func = [](int x, int y) {return x-y;};
    _ids.Put("-", def);

    def = make_shared<List>();
    def->type = List::E_BiFunc;
    def->name = "*";
    def->val.func = [](int x, int y) {return x*y;};
    _ids.Put("*", def);

    def = make_shared<List>();
    def->type = List::E_BiFunc;
    def->name = "/";
    def->val.func = [](int x, int y) {return x/y;};
    _ids.Put("/", def);

    def = make_shared<List>();
    def->type = List::E_Logic;
    def->name = "True";
    def->val.logic = true;
    _ids.Put("True", def);

    def = make_shared<List>();
    def->type = List::E_Logic;
    def->name = "False";
    def->val.logic = false;
    _ids.Put("False", def);
  }

  inline void Preserve(shared_ptr<List> list) {
    if (list->type == List::E_Identifier) {
      auto val = _ids.Get(list->name);
      if (val != nullptr && val->type != List::E_LambdaArg) {
        list->identifier_target = val;
      }
    }
  }

  void TraverseLambda(shared_ptr<List> lmda) {
    TraverseAndPreserve(lmda->right->left);
    if (lmda->left)
      TraverseAndPreserve(lmda->left);
  }

  void TraverseAndPreserve(shared_ptr<List> list) {
    if (list->left == nullptr && list->right == nullptr) {
      Preserve(list);
      return;
    }
    if (list->type == List::E_Lambda) {
      _ids.AddTemp();
      auto x = list->right->right;
      auto para = list->left;
      while (x != nullptr) {
        _ids.Put(x->name, _lmd_arg);
        x = x->left;
      }
      TraverseLambda(list);
      _ids.RemoveTemp();
      return;
    }
    Preserve(list);
    if (list->left) TraverseAndPreserve(list->left);
    if (list->right) TraverseAndPreserve(list->right);
  }

  shared_ptr<List> Eval(shared_ptr<List> list) {
    if (list->right == nullptr) {
      if (list->type == List::E_Identifier) {
        auto value = list->identifier_target;
        if (value == nullptr) {
          value = _ids.Get(list->name);
        }
        if (value == nullptr) {
          throw "undefined identifier " + list->name;
        }
        if (value->type == List::E_Identifier) {
          value = Eval(value);
        }
        return value;
      }
      return list;
    }
    if (list->type == List::E_Define) {
      auto id = list->right;
      auto res = id->left;
      auto value = _ids.Get(id->name);
      if (value != nullptr) {
        throw "identifier \"" + id->name + "\" exists";
      }
      _ids.Put(id->name, Eval(res));
      return list;
    }
    if (list->type == List::E_List) {
      auto first = list->right;
      if (first->type != List::E_BiFunc && first->type != List::E_Lambda) {
        first = Eval(first);
      }
      if (first->type == List::E_BiFunc) {
        auto second = Eval(list->right->left);
        auto third = Eval(list->right->left->left);
        auto res = make_shared<List>();
        res->type = List::E_Number;
        res->val.number = first->val.func(second->val.number, third->val.number);
        return res;
      }
      if (first->type == List::E_Lambda) {
        _ids.AddTemp();
        auto x = first->right->right;
        auto para = list->right->left;
        while (x != nullptr) {
          if (para == nullptr) {
            throw "too few parameters passed";
          }
          auto para_v = Eval(para);
          _ids.Put(x->name, para_v);
          x = x->left;
          para = para->left;
        }
        auto res = Eval(first->right->left);
        _ids.RemoveTemp();
        return res;
      }
      return first;
    }
    if (list->type == List::E_Lambda) {
      _ids.AddTemp();
      auto x = list->right->right;
      auto para = _ids.Get(x->name);
      if (list->left == nullptr || para->type == List::E_LambdaArg) {
        while (x != nullptr) {
          _ids.Put(x->name, _lmd_arg);
          x = x->left;
        }
        TraverseLambda(list);
        _ids.RemoveTemp();
        return list;
      }
      while (x != nullptr) {
        para = _ids.Get(x->name);
        auto para_v = Eval(para);
        _ids.Put(x->name, para_v);
        x = x->left;
        para = para->left;
      }
      auto res = Eval(list->right->left);
      _ids.RemoveTemp();
      return res;
    }
    if (list->type == List::E_Eq) {
      auto arg1 = Eval(list->right);
      auto arg2 = Eval(list->right->left);
      auto tmp = make_shared<List>();
      tmp->type = List::E_Logic;
      tmp->val.number = arg1->val.number == arg2->val.number;
      return tmp;
    }
    if (list->type == List::E_Cond) {
      auto x = list->right;
      while (x != nullptr) {
        auto boo = Eval(x->right);
        auto expr = Eval(x->right->left);
        if (boo->val.logic) return expr;
        x = x->left;
      }
      throw "no True condition";
    }
    if (list->type == List::E_Identifier) {
      return Eval(list);
    }
  }
 public:
  string in, out;
  Lisp() {
    Init();
  }
  string& Exec() {
    auto root = BuildSyntaxTree();
    // PrintTree(root);

    auto res = Eval(root);

    if (res->type == List::E_Number) {
      out = std::to_string(res->val.number);
    } else if (res->type == List::E_Logic) {
      out = res->val.logic ? "True": "False";
    } else {
      out = res->name;
    }
    return out;
  }
 private:
  static void PrintTokens(const vector<shared_ptr<const Token> > &tokens) {
    cout << "tokens:";
    bool flag = false;
    for (auto &x: tokens) {
      if (flag) {
        cout << ", \"" << x->name << "\"";
      } else {
        flag = true;
        cout << "\"" << x->name << "\"";
      }
    }
    cout << endl;
  }
  void PrintTree(shared_ptr<List> root) const {
    cout << "syntax tree (latex tikz-qtree):" << endl << "\\Tree ";
    _PrintTree(root);
    cout << endl;
  }
  void _PrintTree(shared_ptr<List> root) const {
    if (!root->left && !root->right) {
      cout << root->name;
      return;
    }
    cout << "[." << root->name;
    if (root->left) {
      cout << " ";
      _PrintTree(root->left);
    } else {
      cout << " {}";
    }
    if (root->right) {
      cout << " ";
      _PrintTree(root->right);
    } else {
      cout << " {}";
    }

    cout << " ]";
  }
};

int main() {
  std::ios::sync_with_stdio(false);
  Lisp *lisp = new Lisp;
  while (getline(cin, lisp->in)) {
    try {
      cout << lisp->Exec() << endl;
    } catch (const string &e) {
      cout << e << endl;
    }
  }
  delete lisp;
  return 0;
}

// int main() {
//   std::ios::sync_with_stdio(false);
//   Lisp *lisp = new Lisp;

//   vector<string> cmds = {
//     "(define 33g (lambda (x1 x2 x3 x4) ((lambda (x1 x2) "
//     "((lambda (x1 x2 x3) (+ x4 (+ x1 ((lambda (x4 x1) "
//     "(+ (* x4 x3) (* x1 x1))) x2 x3)))) x2 x3 0alpha)) x1 x3)))",
//     "(define 0alpha 666)",
//     "(33g 1 2 3 4)",
//     "(33g 2 4 6 8)",
//     "(33g 0 0 0 0)",
//     "(define 666kk (lambda (x1) (lambda (x2 x3) (+ x1 (+ x2 x3)))))",
//     "((666kk 10) 20 30)"
//   };

//   for (auto & x: cmds) {
//     lisp->in = x;
//     try {
//       cout << lisp->Exec() << endl;
//     } catch (const string &e) {
//       cout << e << endl;
//     }
//   }
//   delete lisp;
//   return 0;
// }
