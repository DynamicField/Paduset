//
// Created by jeuxj on 29/10/2022.
//

#ifndef PADUSET_TOKENS_H
#define PADUSET_TOKENS_H

#include <string>
#include <iostream>
#include <fstream>
#include <utility>
#include <vector>
#include <memory>
#include "utf8.h"

namespace Paduset {
    /**
     * Indicates what the token actually is.
     * KW: Keyword
     * I: Identifier
     * L: Literal
     * OP: Operator
     * S: Separator
     * C: Comment
     */
    enum class TokenKind {
        Empty,
        KW_Program,
        KW_Variables,
        KW_Integer,
        KW_Real,
        KW_Boolean,
        KW_String,
        KW_Begin,
        KW_End,
        KW_Write,
        KW_Read,
        KW_If,
        KW_Else,
        KW_Then,
        I_Identifier,
        I_Garbage,
        L_Integer,
        L_Real,
        L_Bool,
        L_String,
        OP_Add,
        OP_Subtract,
        OP_Multiply,
        OP_Divide,
        OP_IntegerDivide,
        OP_Modulo,
        OP_LowerThan,
        OP_LowerOrEqualThan,
        OP_GreaterThan,
        OP_GreaterOrEqualThan,
        OP_Assign,
        OP_Equals,
        OP_NotEquals,
        OP_Declare,
        S_LeftParens,
        S_RightParens,
        S_Comma,
        S_NewLine,
        C_SingleLine,
        C_MultiLine
    };

    constexpr bool RequiresIntContent(TokenKind K) {
        return K == TokenKind::L_Integer;
    }

    constexpr bool RequiresBoolContent(TokenKind K) {
        return K == TokenKind::L_Bool;
    }

    constexpr bool RequiresFloatContent(TokenKind K) {
        return K == TokenKind::L_Real;
    }

    constexpr bool RequiresStringContent(TokenKind K) {
        return K == TokenKind::L_String || K == TokenKind::I_Identifier || K == TokenKind::I_Garbage;
    }

    constexpr bool RequiresNoContent(TokenKind K) {
        return !RequiresIntContent(K) &&
               !RequiresBoolContent(K) &&
               !RequiresFloatContent(K) &&
               !RequiresStringContent(K);
    }

    constexpr const char* TokenName(TokenKind K) {
        switch (K) {
            case TokenKind::Empty:
                return "<None>";
            case TokenKind::KW_Program:
                return "KW_Program";
            case TokenKind::KW_Variables:
                return "KW_Variables";
            case TokenKind::KW_Integer:
                return "KW_Integer";
            case TokenKind::KW_Real:
                return "KW_Real";
            case TokenKind::KW_Boolean:
                return "KW_Boolean";
            case TokenKind::KW_String:
                return "KW_String";
            case TokenKind::KW_Begin:
                return "KW_Begin";
            case TokenKind::KW_End:
                return "KW_End";
            case TokenKind::KW_Write:
                return "KW_Write";
            case TokenKind::KW_Read:
                return "KW_Read";
            case TokenKind::KW_If:
                return "KW_If";
            case TokenKind::KW_Else:
                return "KW_Else";
            case TokenKind::KW_Then:
                return "KW_Then";
            case TokenKind::I_Identifier:
                return "I_Identifier";
            case TokenKind::I_Garbage:
                return "I_Garbage";
            case TokenKind::L_Integer:
                return "L_Integer";
            case TokenKind::L_Real:
                return "L_Real";
            case TokenKind::L_Bool:
                return "L_Bool";
            case TokenKind::L_String:
                return "L_String";
            case TokenKind::OP_Add:
                return "OP_Add";
            case TokenKind::OP_Subtract:
                return "OP_Subtract";
            case TokenKind::OP_Multiply:
                return "OP_Multiply";
            case TokenKind::OP_Divide:
                return "OP_Divide";
            case TokenKind::OP_IntegerDivide:
                return "OP_IntegerDivide";
            case TokenKind::OP_Modulo:
                return "OP_Modulo";
            case TokenKind::OP_LowerThan:
                return "OP_LowerThan";
            case TokenKind::OP_LowerOrEqualThan:
                return "OP_LowerOrEqualThan";
            case TokenKind::OP_GreaterThan:
                return "OP_GreaterThan";
            case TokenKind::OP_GreaterOrEqualThan:
                return "OP_GreaterOrEqualThan";
            case TokenKind::OP_Assign:
                return "OP_Assign";
            case TokenKind::OP_Equals:
                return "OP_Equals";
            case TokenKind::OP_NotEquals:
                return "OP_NotEquals";
            case TokenKind::OP_Declare:
                return "OP_Declare";
            case TokenKind::S_LeftParens:
                return "S_LeftParens";
            case TokenKind::S_RightParens:
                return "S_RightParens";
            case TokenKind::S_Comma:
                return "S_Comma";
            case TokenKind::S_NewLine:
                return "S_NewLine";
            case TokenKind::C_SingleLine:
                return "C_SingleLine";
            case TokenKind::C_MultiLine:
                return "C_MultiLine";
            default:
                return "Unknown";
        }
    }

    struct CharSpan {
        CharSpan(size_t startChar, size_t endChar);

        size_t startChar;
        size_t endChar;

        CharSpan CombineWith(const CharSpan& other);
    };

    struct Token {
        TokenKind kind = TokenKind::Empty;
        CharSpan span{0, 0};
        union {
            char nothing;
            bool boolValue;
            char charValue;
            int intValue;
            float floatValue;
        } content{};
        std::shared_ptr<const std::string> stringContent;
    public:
        void PrintDebugInformation(std::ostream& stream) const;

        static auto MakeNoContent(TokenKind kind, CharSpan span) -> Token {
            if (!RequiresNoContent(kind)) {
                throw std::runtime_error("Called Token::MakeNoContent with an invalid token kind.");
            }
            return Token{kind, span};
        }

        template<TokenKind K>
        static auto Make(CharSpan span) -> typename std::enable_if<RequiresNoContent(K), Token>::type {
            return Token{K, span};
        }

        template<TokenKind K>
        static auto Make(bool value, CharSpan span) -> typename std::enable_if<RequiresBoolContent(K), Token>::type {
            Token token{K, span};
            token.content.boolValue = value;
            return token;
        }

        template<TokenKind K>
        static auto Make(int value, CharSpan span) -> typename std::enable_if<RequiresIntContent(K), Token>::type {
            Token token{K, span};
            token.content.intValue = value;
            return token;
        }

        template<TokenKind K>
        static auto Make(float value, CharSpan span) -> typename std::enable_if<RequiresFloatContent(K), Token>::type {
            Token token{K, span};
            token.content.floatValue = value;
            return token;
        }

        template<TokenKind K>
        static auto Make(const std::string& value, CharSpan span)
        -> typename std::enable_if<RequiresStringContent(K), Token>::type {
            Token token{K, span};
            token.stringContent = std::make_shared<std::string>(value);
            return token;
        }

        explicit operator bool() const {
            return kind != TokenKind::Empty;
        }

        const Token& operator||(const Token& rhs) const {
            if (kind != TokenKind::Empty) {
                return *this;
            } else {
                return rhs;
            }
        }
    };

    static const Token EmptyToken{TokenKind::Empty, {0, 0}};

    struct TokenSpan {
        TokenSpan() : start(EmptyToken), end(EmptyToken) {}

        explicit TokenSpan(const Token& both) : start(both), end(both) {}

        TokenSpan(Token start, Token anEnd) : start(std::move(start)), end(std::move(anEnd)) {}

        Token start;
        Token end;

        TokenSpan CombineWith(const TokenSpan& other) const;

        CharSpan ToCharSpan() const;

        CharSpan BeginningCursor() const;

        CharSpan EndingCursor() const;
    };

    class Tokenizer {
    public:
        explicit Tokenizer(std::ifstream&& stream) : stream(std::move(stream)) {
            streamIter = std::istreambuf_iterator(this->stream.rdbuf());
        }

        const std::vector<Token>& Read();

        void PrintDebugInformation(std::ostream& debugStream);

    private:
        bool NextChar();

        uint32_t PeekChar();

        bool SkipChar() const;

        bool RecognizeNumber(Token& token);

        bool RecognizeKeyword(Token& token);

        bool RecognizeOperator(Token& token);

        bool RecognizeSeparator(Token& token);

        bool RecognizeStringLiteralStart() const;

        bool RecognizeEscapeSequenceStart() const;

        bool RecognizeStringLiteralEnd(Token& token);

        bool IsValidIdentifierCharacter(uint32_t ch);

        void AddToken(const Token& token);

        void MergeCompositeTokens();

        void FlushIdentifier();

        CharSpan GetWordSpan(size_t offsetStart = 0, size_t offsetEnd = 0);

        void UpdateWordCharSpan();

        int UtfCharacterBytes(uint32_t ch);

        std::ifstream stream;
        std::istreambuf_iterator<char> streamIter;
        std::istreambuf_iterator<char> streamIterEnd{};

        std::vector<Token> tokens{};
        std::vector<std::string> consecutiveLowerCaseWords{};

        uint32_t character = '\0';
        int characterBytes = 0;

        uint32_t pendingNextCharacter = '\0';
        int pendingNextCharacterBytes = 0;

        std::string word;
        size_t wordStartCharIndex = 0;
        size_t wordEndCharIndex = 0;

        std::string wordLowerCase;
        bool inStringLiteral = false;
        bool inEscapeSequence = false;

        int identifierCharCount = 0;
    };
}
#endif //PADUSET_TOKENS_H
